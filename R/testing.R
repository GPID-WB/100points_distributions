# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# project:       test 100-point distribution
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Creation Date:    2023-01-11
# References:
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                   Load Libraries   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(data.table)
library(janitor)
library(here)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Set up   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wd <- qs::qread("data/album/world_100bin_2017PPP.qs")
setorderv(wd,
          c(
            "country_code",
            "reporting_level",
            "welfare_type",
            "year",
            "bin"
          ))

vars  <- c("avg_welfare", "welfare_share", "quantile")
dvars <- paste0("d", vars)

wd[,
  (dvars) := lapply(.SD, \(.x) {
                  y   <- .x - shift(.x)
                  tag <- y < 0
                  tag
                }
                ),
  by = .(country_code,
        reporting_level,
        welfare_type,
        year),
  .SDcols = vars]


x <- dvars[1]
wd[get(x) == TRUE]


pr <- wd[get(x) == TRUE, c("country_code", "year")] |>
  unique()



pfw <- pipload::pip_load_aux("pfw")
pfw[, year := as.character(year)]

dt <-
  merge(x = pr,
        y = pfw,
        by = c("country_code", "year"))

use_vars <- grep("^use_", names(dt), value = TRUE) |>
  c("country_code", "year")

dt[, ..use_vars]



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load data   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


pfw <- pipload::pip_load_aux("pfw")
pfw[, year := as.character(year)]

pfw <-
  merge(x = pr,
        y = pfw,
        by = c("country_code", "year"))



fpf <- pfw[, .(country_code,
               surveyid_year,
               reporting_year,
               welfare_type
)]

fpf[,
    id := paste(country_code, reporting_year, welfare_type, sep = "_")
]


# Testing: here we have to include some code that allows the use to select
# different countries, years, etc.
# st <- sample(nrow(fpf), size = 3, replace =FALSE)
# fpf <- fpf[st]


# fpf <- fpf[country_code %in% c("SOM", "COL") & surveyid_year == 2017]

fpf <- fpf |>
  split(by = "id")


pl <- fpf[[1]]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
