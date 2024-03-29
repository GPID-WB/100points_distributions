
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run initial conditions   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("R/init.R")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load data   ---------
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



# Testing: here we have to include some code that allows the use to select
# different countries, years, etc.
# st <- sample(nrow(fpf), size = 3, replace =FALSE)
# fpf <- fpf[st]


# fpf <- fpf[country_code %in% c("SOM", "COL") & surveyid_year == 2017]
# fpf <- fpf[country_code %in% c("POL") & surveyid_year == 2004]

fpf <- fpf |>
  split(by = "id")

poss_get_bin_dist <- purrr::possibly(.f = get_micro_dist,
                                     otherwise = NULL)
dr <- purrr::map(.x = fpf,
                 .f = poss_get_bin_dist)

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
