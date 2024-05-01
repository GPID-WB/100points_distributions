
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# project:       Create 100-point distribution for group data
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Creation Date:    2022-12-06
# Modification Date:
# Script version:    01
# References:
#
#
# Output:             data.table with distribution
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run initial conditions   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("R/init.R")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# load Aux data   ---------
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

# |>
#   funique(c("country_code", "reporting_year", "reporting_level", "cpi")) |>
#   setorder(country_code, reporting_year, reporting_level) |>
# # to merge with GDM
#   ftransform(rep_level = rowid(country_code,
#                                reporting_year))

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


## merge aux data ----------------

### CPI and PFW -----------
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

### Add GDM --------------
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

### Add PPP ------------------
dt <-
  joyn::merge(gdm_cpi_pfw, ppp,
              by = c("country_code", "rep_level"),
              match_type = "m:1",
              # reportvar = FALSE,
              keep = "left",
              reportvar = FALSE) # should all match


# deflate mean   ---------
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



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get population and welfare vctrs for Group data   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
issues <- c('CHN_2021_consumption', 'QAT_2017_income')


fpf <- pfw[,.(country_code, #temporary just to run the code
               surveyid_year,
               reporting_year,
               survey_year,
               welfare_type)]

fpf[, `:=`(
    id = paste(country_code, reporting_year, welfare_type, sep = "_"),
    version = version
  )
]

fpf <- fpf[id %!in% issues,]

# fpf <- fpf[1:5]
# lf <- as.list(fpf)
pl <- split(fpf, by = "id")

vctrs <- map(pl, gd_pop_wlf)
names(vctrs) <- fpf[, id]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get distributions   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rd <-
  map(.x = names(vctrs),
             .f = ~{
               id <- .x
               y <- mean_ppp[[id]]
               v <- vctrs[[id]]

               levels <- names(y)

               map(.x   = levels,
                   .f   = poss_get_gd_calcs,
                   vctr = v,
                   mean = y,
                   id   = id) |>
               rbindlist()
             })

names(rd) <- names(vctrs)

# Problematic databases
rd_err <-
  rd |>
  keep(is.null) |>
  names()

rd_err

# Get rid of problematic data
rd <-
  compact(rd) |>
  map(.f = ~{
    .x[,
      welfare_type := gsub("(.+_)([^_]+)$", "\\2", id)
    ]
  })


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# format and save data   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

iwalk(rd, \(x, idx) fmt_sve(x, idx))

# rd <- rbindlist(rd, use.names = TRUE)
