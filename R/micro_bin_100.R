
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run initial conditions   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("R/init.R")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load data   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


pfw <- pipload::pip_load_aux("pfw") |>
  {\(.) .[use_bin == 1 || use_microdata == 1]}()


fpf <- pfw[, .(country_code,
               surveyid_year,
               reporting_year,
               welfare_type
)]

fpf[,
    id := paste(country_code, reporting_year, welfare_type, sep = "_")
]

# fpf <- fpf[1:2]
fpf <- fpf |>
  split(by = "id")


poss_get_bin_dist <- purrr::possibly(.f = get_micro_dist,
                                     otherwise = NULL)
dr <- purrr::map(.x = fpf,
                 .f = poss_get_bin_dist)

# Problematic databases
dr_err <-
  dr |>
  purrr::keep(is.null) |>
  names()
dr_err



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# format and save data   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

walk(dr, fmt_sve)

# rd <- rbindlist(rd, use.names = TRUE)
