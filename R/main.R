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
  "20250930_2021_01_02_PROD",
  "20250930_2017_01_02_PROD",
  "20250930_2021_01_02_PROD",
  "20250930_2017_01_02_PROD")

# nqs  <- 100
# vers <- c(
#   "20250401_2021_01_02_PROD")
#

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
  source("R/copy_to_p.R")

  options(op)
}

if (require(pushoverr)) {
  msg <- paste("Done creating singles and album for", version, "and", nq, "bins",
               sep = " ")
  pushoverr::pushover(msg)
}



## !!! folders parameters: change to desired folder in pip
version <- "20250401_2021_01_02_PROD"

album_dir <-
  fs::path("data/album", version)

singles_dir <-
  fs::path("data/singles", version)


wld <- fs::path(album_dir, "world_100bin.qs") |>
  qs::qread()

# sorting vars
svars <- c("country_code", "reporting_level", "welfare_type", "year", "percentile")

# Grouping vars
gvars <- c("country_code", "reporting_level", "welfare_type", "year")
setorderv(wld,svars)

g <- GRP(wld, gvars)

failing <-
  wld |>
  ftransform(diff = welfare_share - flag(welfare_share, g = g, t = percentile)) |>
  # fsubset(country_code %in% c("CHN", "ARG", "AGO")) |>
  ftransform(tag = diff < -1e-8) |>
  fsubset(tag == TRUE) |>
  _[, ..gvars] |>
  unique()

failing[]







wld |>
  fsubset(country_code %in% c("CHN", "ARG", "AGO")) |>
  ftransform(diff = welfare_share - flag(welfare_share,
                                         g = reporting_level)) |>
  ftransform(tag = diff < 0) |>
  fsubset(country_code == "ARG" & year == 1997)



#
# nqs  <- c(100, 100, 1000, 1000)
# vers <- c(
#   "20250401_2021_01_02_PROD",
#   "20250401_2017_01_02_PROD",
#   "20250401_2021_01_02_PROD",
#   "20250401_2017_01_02_PROD")
#
#
# dok <- vector("list", length = length(nqs))
# for (i in seq_along(nqs)) {
#   nq <- nqs[i]
#   version <- vers[i]
#
#   singles_dir <-
#     fs::path("data/singles", version) |>
#     fs::dir_create()
#
#
#   files <- fs::dir_ls(singles_dir, regexp = paste0(nq, "bin"))
#   dok[[i]] <-
#     lapply(files, \(x) {
#     ws_ok <- qs::qattributes(x)$welfare_share_OK
#     data.table(version = version, file = fs::path_file(x), ws_ok = ws_ok)
#   }) |>
#     rowbind()
# }
#
# all_ok <- rowbind(dok)
#
# all_ok[ws_ok == FALSE]
