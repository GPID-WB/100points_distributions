# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# project:       Create 100-point distribution
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


nqs  <- c(100, 100, 1000, 1000)
vers <- c(
  "20250401_2021_01_02_PROD",
  "20250401_2017_01_02_PROD",
  "20250401_2021_01_02_PROD",
  "20250401_2017_01_02_PROD")


for (i in seq_along(nqs)) {
  nq <- nqs[i]
  version <- vers[i]

  popshare <- seq(from = 1 / nq,
                  to = 1,
                  by = 1 / nq)



  source("R/init.R")
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Get 100 bin per data type   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # source("R/gd_100.R")
  source("R/micro_bin_100.R")

  # source("R/rur_urb_100_national.R")
  source("R/append_singles.R")
  source("R/labels_stata.R")

  if (require(pushoverr)) {
    msg <- paste("Done creating singles and album for", version, "and", nq, "bins",
                 sep = " ")
    pushoverr::pushover(msg)
  }

  source("R/copy_to_p.R")
  options(op)
}

