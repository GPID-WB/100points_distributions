
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
# Notes: This is a draft of the final pipeline, it calculates
# distributions for CHN, IND and IDN before 2021 only.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. Run initial conditions   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("R/init_national.R")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. Load Aux data   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## PFW ------------
pfw <- pipload::pip_load_aux("pfw") |>
  fsubset(use_groupdata == 1 &
            inpovcal  == 1) |>
  fselect(country_code,
          reporting_year,
          survey_year,
          surveyid_year,
          survey_acronym,
          welfare_type,
          tosplit)


## CPI ------------
cpi_var <- paste0("cpi", ppp_year)
cpi <- pipload::pip_load_aux("cpi") |>
  ftransform(cpi = get(cpi_var)) |>
  fselect(country_code,
          survey_year,
          surveyid_year  = cpi_year,
          reporting_level = cpi_data_level,
          survey_acronym,
          cpi)

## PPP -------------------
ppp <- pipload::pip_load_aux("ppp")

ppp <- ppp |>
  fsubset(ppp_year == py &
            ppp_default_by_year == TRUE) |>
  fselect(country_code,
          reporting_level = ppp_data_level,
          ppp) |>
  setorder(country_code, reporting_level) |>
  ftransform(rep_level = rowid(country_code))

## GDM ---------------
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

## POP ----------------
pop <- pipload::pip_load_aux("pop") |>
  fselect(country_code,
          reporting_year = year,
          reporting_level = pop_data_level,
          pop) |>
  setorder(country_code, reporting_year, reporting_level) |>
  ftransform(rep_level = rowid(country_code, reporting_year))


# 3. Merge aux data ----------------

## CPI and PFW -----------
cpi_pfw <-
  joyn::merge(cpi, pfw,
              by = c("country_code",
                     "survey_year",
                     "surveyid_year",
                     "survey_acronym"),
              keep = "inner",
              reportvar = FALSE) |>  # There should not be "y"
  setorder(country_code, survey_year, surveyid_year, reporting_level) |>
  ftransform(rep_level = rowid(country_code, survey_year))

## Add GDM --------------
gdm_cpi_pfw <-
  joyn::merge(gdm, cpi_pfw,
              by = c("country_code",
                     "survey_year",
                     "surveyid_year",
                     "rep_level",
                     "welfare_type"),
              match_type = "1:1")


vars <- names(gdm_cpi_pfw) |>
  copy()

vars <- vars[vars %!in% c("country_code", "survey_year", "report")]

# Use national cpi for for those values with NA in urban
gdm_cpi_pfw[,
            (vars) := lapply(.SD,
                             \(x) {
                               fifelse(is.na(x) & report == "x",
                                       flag(x), x)
                             }),
            .SDcols = vars,
            by = c("country_code", "survey_year")
][,
  report := NULL]

## Add PPP ------------------
dt <-
  joyn::merge(gdm_cpi_pfw, ppp,
              by = c("country_code", "rep_level"),
              match_type = "m:1",
              # reportvar = FALSE,
              keep = "left",
              reportvar = FALSE) # should all match

## Deflate mean   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dt[, mean_ppp :=  {
  x <- deflate_welfare_mean(survey_mean_lcu,
                            ppp,
                            cpi)
  x <- x/(365/12)
}
][,
  id := paste(country_code, reporting_year, welfare_type, sep = "_")
]


mean_ppp <-
  dt[!is.na(mean_ppp)
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


# 4. Create pop list with IDs ----
dt_ids <-dt[,
            c('country_code', 'reporting_year', 'id')]


pop_list <-
  joyn::merge(pop, dt_ids,
              reportvar = FALSE,
              keep = 'right') |> # assuming that these are the data we need
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
# 5. Get population and welfare vctrs for CHN, IND and IDN ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# !!!! CHN, IND, IDN filter ----
countries <- c("CHN", "IND", "IDN")

fpf <- pfw[country_code %in% countries
           & survey_year < 2021, # temporary because CHN 2021 does not have reporting_year
           .(country_code,
             surveyid_year,
             reporting_year,
             survey_year,
             welfare_type)]


fpf[, `:=`(
  id = paste(country_code, reporting_year, welfare_type, sep = "_"),
  version = version
)
]

pl <- split(fpf, by = "id")

vctrs <- map(pl, gd_pop_wlf)
names(vctrs) <- fpf[, id]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 6. Get distributions at national level   ----------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

outer_wf <- map(
  .x = names(vctrs),
  .f = ~{
    # Extract details for the current id
    id <- .x
    mean_id <- mean_ppp[[id]]
    vctr_id <- vctrs[[id]]
    pop_table_id <- pop_list[[id]]

    # Get the names of the levels
    levels <- names(mean_id)

    # Map over levels and apply the get_synth_level function
    wf_id <- purrr::map(
      .x = levels,
      .f = ~get_synth_level(
        level = .x,
        vctr = vctr_id,
        mean = mean_id,
        pop = pop_table_id
      )
    ) |>
      rbindlist() |>
      setorder(welfare)
  }
)

# Assign the names from vctrs to the outer_wf result
names(outer_wf) <- names(vctrs)

