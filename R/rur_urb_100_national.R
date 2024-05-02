
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# project:       Create 100-point national distribution for group data
# Author:        Giorgia
# Dependencies:
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Creation Date:    2024-04-30
# Modification Date:
# Script version:    01
# References:
#
#
# Output:             data.table with distribution
# Notes:
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. Run initial conditions   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("R/init.R")

# Filter for specific countries  ----
#countries <- c("CHN", "IND", "IDN")

# 2. GROUPED DATA ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 2.1 Load Aux data   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### PFW ------------
pfw_gd <- pipload::pip_load_aux("pfw") |>
  fsubset(use_groupdata == 1 & # filter for grouped data
            inpovcal  == 1) |> # do I need to keep this?
  fselect(country_code,
          reporting_year,
          survey_year,
          surveyid_year,
          survey_acronym,
          welfare_type,
          tosplit)


### CPI ------------
cpi_var <- paste0("cpi", ppp_year)
cpi <- pipload::pip_load_aux("cpi") |>
  ftransform(cpi = get(cpi_var)) |>
  fselect(country_code,
          survey_year,
          surveyid_year  = cpi_year,
          reporting_level = cpi_data_level,
          survey_acronym,
          cpi)

### PPP -------------------
ppp <- pipload::pip_load_aux("ppp")

ppp <- ppp |>
  fsubset(ppp_year == py &
            ppp_default_by_year == TRUE) |>
  fselect(country_code,
          reporting_level = ppp_data_level,
          ppp) |>
  setorder(country_code, reporting_level) |>
  ftransform(rep_level = rowid(country_code))

### GDM ---------------
gdm <- pipload::pip_load_aux("gdm") |>
  fselect(country_code,
          survey_year,
          surveyid_year,
          welfare_type,
          survey_mean_lcu,
          distribution_type,
          reporting_level = pop_data_level) |>
  setorder(country_code, survey_year, surveyid_year, reporting_level) |>
  ftransform(rep_level = rowid(country_code, survey_year))

### POP ----------------
pop <- pipload::pip_load_aux("pop") |>
  fselect(country_code,
          reporting_year = year,
          reporting_level = pop_data_level,
          pop) |>
  setorder(country_code, reporting_year, reporting_level) |>
  ftransform(rep_level = rowid(country_code, reporting_year))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 2.2 Merge Aux data   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### CPI and PFW -----------
cpi_pfw_gd <-
  joyn::merge(cpi, pfw_gd,
              by = c("country_code",
                     "survey_year",
                     "surveyid_year",
                     "survey_acronym"),
              keep = "inner",
              reportvar = FALSE) |>  # There should not be "y"
  setorder(country_code, survey_year, surveyid_year, reporting_level) |>
  ftransform(rep_level = rowid(country_code, survey_year))

### Add GDM --------------
gdm_cpi_pfw_gd <-
  joyn::merge(gdm, cpi_pfw_gd,
              by = c("country_code",
                     "survey_year",
                     "surveyid_year",
                     "rep_level",
                     "welfare_type"),
              match_type = "1:1")


vars <- names(gdm_cpi_pfw_gd) |>
  copy()

vars <- vars[vars %!in% c("country_code", "survey_year", "report")]

### Use national cpi for for those values with NA in urban
gdm_cpi_pfw_gd[,
            (vars) := lapply(.SD,
                             \(x) {
                               fifelse(is.na(x) & report == "x",
                                       flag(x), x)
                             }),
            .SDcols = vars,
            by = c("country_code", "survey_year")
][,
  report := NULL]

### Add PPP ------------------
dt_gd <-
  joyn::merge(gdm_cpi_pfw_gd, ppp,
              by = c("country_code", "rep_level"),
              match_type = "m:1",
              # reportvar = FALSE,
              keep = "left",
              reportvar = FALSE) # should all match

### Deflate mean   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dt_gd[, mean_ppp :=  {
  x <- deflate_welfare_mean(survey_mean_lcu,
                            ppp,
                            cpi)
  x <- x/(365/12)
}
][,
  id := paste(country_code, reporting_year, welfare_type, sep = "_")
]


mean_ppp <-
  dt_gd[!is.na(mean_ppp)
  ][,  # keep no na data
    # keep important variables
    c("id", "mean_ppp", "reporting_level")
  ] |>
  # create list of means and reporting level by id
  split(by = "id",
        keep.by = FALSE) |>
  # convert data.table into vectors of means with reporting levels as names
  map(~{
    y        <- .x[, mean_ppp]
    names(y) <- .x[, reporting_level]
    attr(y,"label") <- NULL
    y
  })


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 2.3 Create pop_list with id   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dt_ids_gd <-dt_gd[,
            c('country_code', 'reporting_year', 'id')]


pop_list_gd <-
  joyn::merge(pop, dt_ids_gd,
              reportvar = FALSE,
              keep = 'right') |>
  unique() |>
  split(by=c('id'),
        keep.by = FALSE) |>
  map(~{
    pop <- .x[, pop]
    names(pop) <- .x[, reporting_level]
    attr(pop, 'label') <- NULL
    pop
  })

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.4 Get population and welfare vctrs  ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fpf_gd <- pfw_gd[,
                  .(country_code,
                    surveyid_year,
                    reporting_year,
                    survey_year,
                    welfare_type)]


fpf_gd[, `:=`(
  id = paste(country_code, reporting_year, welfare_type, sep = "_"),
  version = version
)
]

issues <- c('CHN_2021_consumption', 'QAT_2017_income') # they don't have reporting_level?
fpf_gd <- fpf_gd[id %!in% issues,]

pl <- split(fpf_gd, by = "id")

vctrs <- map(pl, gd_pop_wlf_rur_urb)
names(vctrs) <- fpf_gd[, id]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.5 Get distributions at national level   ---------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Map
gd_synth_100 <- map(
  .x = names(vctrs),
  .f = ~{
    id <- .x
    mean_id <- mean_ppp[[id]]
    vctr_id <- vctrs[[id]]
    pop_table_id <- pop_list_gd[[id]]


    wf_id <- poss_create_synth_bins(
      vctr = vctr_id,
      mean = mean_id,
      pop = pop_table_id,
      nbins = 100
    )

    return(wf_id)
  }
)

# Set names for the list of processed data tables
names(gd_synth_100) <- names(vctrs)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 3.3 Null databases   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gd_synth_100_err <-
  gd_synth_bins |>
  keep(is.null) |>
  names()

# NULL databases are the ones with 'national' in it,
# but I need to add a check because there might be
# some databases which fail after.
gd_synth_100_err


# 3. MICRO DATA ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 3.1 Load data   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


pfw <- pipload::pip_load_aux("pfw") |>
  {\(.) .[use_groupdata != 1]}()


fpf <- pfw[, .(country_code,
               surveyid_year,
               reporting_year,
               welfare_type
)]

fpf[, `:=`(
  id = paste(country_code, reporting_year, welfare_type, sep = "_"),
  version = version,
  wt_call = toupper(substr(welfare_type, 1, 3)))
]

fpf <- fpf[reporting_year == 2023,]

fpf <- fpf |>
  split(by = "id")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 3.2 Calculate measures   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
poss_get_micro_dist_rur_urb <- purrr::possibly(.f = get_micro_dist_rur_urb,
                                     otherwise = NULL)
md_100 <- purrr::map(.x = fpf,
                 .f = poss_get_micro_dist_rur_urb)

names(md_100) <- names(fpf)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 3.3 Null databases   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
md_100_err <-
  md_100 |>
  purrr::keep(is.null) |>
  names()

md_100_err

