

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                   Load Libraries   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# pak::pak("PIP-Technical-team/wbpip@refactor_gd")

library(fastverse)
library(wbpip)
library(purrr)
library(glue)

# pipfun::pipinstall("pipfun", "ongoing", dependencies = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# run functions   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source("R/functions.R")
source("R/duplicate_households.R")
op <- options(joyn.reportvar = "report")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                   Initial parameters   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lorenz   <- NULL


# ppp_year <- py <- 2011
ppp_year <- py <- version |>
  gsub("(.*)_([0-9]{4})(_.*)", "\\2", x = _) |>
  as.numeric()


# aux versions
# necessary because CHN 2021 and QAT 2017 not in data.
pfw_version <- '20240229112650'
pfw_version <- NULL

cpi_version <- '20240301133652'
cpi_version <- NULL

ppp_version <- '20230910033743'
ppp_version <- NULL

gdm_version <- '20240312225517'
gdm_version <- NULL

pop_version <- '20240307121122'
pop_version <- NULL


## !!! folders parameters: change to desired folder in pip
singles_dir <-
  fs::path("data/singles", version) |>
  fs::dir_create()

album_dir <-
  fs::path("data/album", version) |>
  fs::dir_create()

ext <- "qs"

