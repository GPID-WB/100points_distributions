
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run initial conditions   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("R/init.R")


cache_inv <- pipload::pip_find_cache(version = version) |>
  {\(.) .[!grepl("GROUP", .)]}() |>
  data.table(cache_id = _)

id_vars <-  c(
  "country_code",
  "surveyid_year",
  "survey_acronym",
  "data_level",
  "welfare_type",
  "source"
)

cache_inv[, (id_vars) := tstrsplit(cache_id, split = "_", fill = TRUE)
          ][, welfare_type := fifelse(welfare_type == "CON", "consumption", "income")]



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load data   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pfw <- pipload::pip_load_aux("pfw")

fpf <- joyn::joyn(cache_inv, pfw,
                  by = c("country_code", "surveyid_year", "survey_acronym", "welfare_type"),
                  keep = 'left',
                  reportvar = FALSE) |>
  fselect(country_code,
          surveyid_year,
          reporting_year,
          welfare_type)


fpf[is.na(reporting_year),
    reporting_year := as.double(surveyid_year)
    ][, `:=`(
  id = paste(country_code, reporting_year, welfare_type, sep = "_"),
  version = version,
  wt_call = toupper(substr(welfare_type, 1, 3)))
  ]



# Testing: here we have to include some code that allows the use to select
# different countries, years, etc.
# st <- sample(nrow(fpf), size = 3, replace =FALSE)
# fpf <- fpf[st]


# fpf <- fpf[country_code %in% c("SOM", "COL") & surveyid_year == 2017]
# fpf <- fpf[country_code %in% c("POL") & surveyid_year == 2004]
# fpf <- fpf[country_code %in% c("POL")]

fpf <- fpf |>
  split(by = "id")

poss_get_micro_dist <- purrr::possibly(.f = get_micro_dist,
                                     otherwise = NULL)
dr <- purrr::map(.x = fpf,
                 .f = poss_get_micro_dist,
                 .progress = TRUE)

names(dr) <- names(fpf)

# to test individual calls
# dr2 <- purrr::map(.x = list(fpf$ALB_2016_income),
#                   .f = get_micro_dist)

# Problematic databases
dr_err <-
  dr |>
  purrr::keep(is.null) |>
  names()
dr_err

dr <-
  dr |>
  purrr::keep(.p = ~{!is.null(.x)})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# format and save data   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

iwalk(dr, \(x, idx) fmt_sve(x, idx))

# rd <- rbindlist(rd, use.names = TRUE)
