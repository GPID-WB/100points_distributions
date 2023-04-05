

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                   Load Libraries   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(data.table)
library(wbpip)
library(purrr)
library(glue)

# pipfun::pipinstall("pipfun", "ongoing", dependencies = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# run functions   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source("R/functions.R")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                   Initial parameters   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nq       <- 100
lorenz   <- NULL
popshare <- seq(from = 1/nq, to = 1, by = 1/nq)
version  <- "20230328_2011_02_02_PROD"
# ppp_year <- py <- 2011
ppp_year <- py <- version |>
  gsub("(.*)_([0-9]{4})(_.*)", "\\2", x = _) |>
  as.numeric()


