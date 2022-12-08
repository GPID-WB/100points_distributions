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
#                   Load Libraries   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(data.table)
library(dtplyr)
library(tidyverse)
library(here)
library(wbpip)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                   Subfunctions   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rename_data_level_var <- function(dt,
                                  verbose = TRUE) {

#   ____________________________________________________
#   Defenses                                        ####
  stopifnot( exprs = {
      is.data.frame(dt)
    }
  )

  if (is.data.table(dt)) {
    df <- copy(dt)
  } else {
    df <- as.data.table(dt)
  }

  vars <- names(df)

  dl_var <- grep("data_level", vars, value = TRUE)


#   ____________________________________________________
#   Early returns                                   ####
  ldl <- length(dl_var)
  if (ldl == 0) {
    if (verbose)
      cli::cli_alert_warning("no {.code data_level} variable found")

    return(dt)
  } else if (ldl > 1) {
    if (verbose)
      cli::cli_alert_warning("Found {ldl} {.code data_level} variable{?s}: {.field {dl_var}}.
                           Rename only works when one variable is found")
    return(dt)
  }

#   ____________________________________________________
#   Computations                                     ####
  setnames(df, dl_var, "data_level")
  df[, data_level := tolower(data_level)]


#   ____________________________________________________
#   Return                                           ####
  return(df)

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                   Initial parameters   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ppp_year <- py <- 2017
nq       <- 10
lorenz   <- NULL
popshare <- seq(from = 1/nq, to = 1, by = 1/nq)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# load Aux data   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


pfw <- pipload::pip_load_aux("pfw") |>
  rename_data_level_var()

gdm <- pipload::pip_load_aux("gdm") |>
  rename_data_level_var()



cpi <- pipload::pip_load_aux("cpi") |>
  rename_data_level_var()

cpi_var <- paste0("cpi", ppp_year)
cpi[, cpi := get(cpi_var)]


ppp <- pipload::pip_load_aux("ppp") |>
  {\(.) .[ppp_year == py &
            ppp_default_by_year  == TRUE]}() |>
  rename_data_level_var()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Id  variables --------

pfw_id <-
  c("country_code",
     "year",
     "reporting_year",
     "surveyid_year",
     "survey_year",
     "welfare_type",
    "survey_acronym")

cpi_id <-
  c("country_code",
    "survey_year",
    "cpi_year",
    "data_level",
    "survey_acronym")


gdm_id <-
  c("country_code",
    "data_level",
    "survey_year",
    "welfare_type")

ppp_id <- c("country_code", "data_level")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Merge aux data   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

by_cpi_ppp     <- intersect(ppp_id, cpi_id)

by_cpi_ppp_gdm <-
  c(ppp_id, cpi_id) |>
  unique() |>
  intersect(gdm_id)

by_cpi_ppp_ggm_pfw <-
  c(cpi_id, ppp_id, gdm_id) |>
  unique() |>
  intersect(pfw_id)

aux <-
  merge(x = cpi,
        y = ppp,
        by = by_cpi_ppp) |>
  merge(gdm,
        by = by_cpi_ppp_gdm)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


pfw <- pipload::pip_load_aux("pfw") |>
  {\(.) .[use_groupdata == 1]}()

gdm <- pipload::pip_load_aux("gdm")

cpi <- pipload::pip_load_aux("cpi")

ppp <- pipload::pip_load_aux("ppp")



get_mean_ppp <- function(ppp_year) {
  gdm <- pipload::pip_load_aux("gdm")
  cpid <- pipload::pip_load_aux("cpi")


  py <- ppp_year # to avoid issues with var name in pppd
  pppd <- pipload::pip_load_aux("ppp")  |>
    {\(.) .[ppp_year == py &
              ppp_default_by_year  == TRUE]}()


  dt <-
    merge(gdm, cpid,
          by.x =  c("country_code", "survey_year", "pop_data_level"),
          by.y =  c("country_code", "survey_year", "cpi_data_level")
    ) |>
    merge(pppd,
          by.x = c("country_code", "pop_data_level"),
          by.y = c("country_code", "ppp_data_level"))

  cpi_var <- paste0("cpi", ppp_year)

  dt[, cpi := get(cpi_var)
  ][, mean_ppp :=  {
    x <- deflate_welfare_mean(survey_mean_lcu,
                              ppp,
                              cpi)
    x <- x/(365/12)
  }
  ][,
    id := paste(country_code, surveyid_year, welfare_type, sep = "_")
  ]

  ft <- dt[!is.na(mean_ppp) ][,  # keep no na data
    # keep important variables
    c("id", "mean_ppp", "pop_data_level")
  ] |>
    # create list of means and reporting level by id
    split(by = "id",
          keep.by = FALSE) |>
    # convert data.table into vectors of means with reporting levels as names
    purrr::map(~{
      y        <- .x[, mean_ppp]
      names(y) <- .x[, pop_data_level]
      attr(y,"label") <- NULL
      y
    })

  return(ft)
}


mean_ppp <- get_mean_ppp(ppp_year)
# mean_ppp <- purrr::pmap(lf, get_mean_ppp)
# mean_ppp <- mean_ppp[[1]]

fpf <- pfw[, .(country_code,
               year,
               surveyid_year,
               reporting_year,
               survey_year,
               pop_domain,
               welfare_type
)]

fpf[,
    id := paste(country_code, surveyid_year, welfare_type, sep = "_")
]

# fpf <- fpf[1:5]

lf <- as.list(fpf)


get_vctrs <- function(...) {
  # pl <- as.list(environment())
  pl <- list(...)
  dt   <- pipload::pip_load_cache(pl$country_code,
                                  pl$surveyid_year,
                                  verbose = FALSE)

  levels     <- dt[, unique(reporting_level)]
  welfare    <- vector("list", length(levels))
  population <- vector("list", length(levels))
  for (i in seq_along(levels)) {
    nn <- levels[[i]]
    welfare[[i]] <- dt[reporting_level == nn,
                       welfare]
    population[[i]] <- dt[reporting_level == nn,
                          weight]
  }

  names(welfare) <- names(population) <- levels

  id <- paste(pl$country_code,
              pl$surveyid_year,
              pl$welfare_type,
              sep = "_")

  # attr(welfare, "id") <- attr(population, "id") <- id


  return(list(welfare    = welfare,
              population = population))
}

vctrs <- purrr::pmap(lf, get_vctrs)

names(vctrs) <- fpf[, id]
# ww <- ww[[1]]

get_calcs <- function(level, vctr, mean, id) {

  welfare    <- vctr$welfare[[level]]
  population <- vctr$population[[level]]
  mean       <- mean[[level]]

  params <- get_gd_quantiles(welfare,
                             population,
                             complete = TRUE,
                             mean     = mean,
                             popshare = popshare,
                             lorenz = lorenz)

  povlines <- params$dist_stats$quantiles
  lorenz   <- params$selected_lorenz$for_dist


  wlf_share <-
    get_gd_wlf_share_by_qtl(params = params,
                            lorenz = lorenz,
                            n      = nq) |>
    {\(.) .$dist_stats$welfare_share}()

  pop_share <- c(popshare[1], diff(popshare))

  avg_wlf_qtl <- (wlf_share*mean)/pop_share

  dt <- data.table(
    quantile        = povlines,
    welfare_share   = wlf_share,
    pop_share       = pop_share,
    avg_welfare     = avg_wlf_qtl,
    reporting_level = level,
    id              = id
  )

  dt[, bin := .I
  ][bin == max(bin),
    quantile := NA_real_]
  return(dt)
}

poss_get_calcs <- purrr::possibly(.f = get_calcs,
                                  otherwise = NULL)

rd <-
  purrr::map(.x = names(vctrs),
             .f = ~{
               id <- .x
               y <- mean_ppp[[id]]
               v <- vctrs[[id]]

               levels <- names(y)
               purrr::map_df(.x = levels,
                             .f = poss_get_calcs,
                             vctr = v,
                             mean = y,
                             id   = id)
             })

# Problematic databases
rd_err <-
  rd |>
  purrr::keep(is.null) |>
  names()
rd_err

# Get rid of problematic data
rd <- purrr::compact(rd)
rd <- rbindlist(rd, use.names = TRUE)

nvars <- c("country_code", "year", "welfare_type")

rd[, (nvars) := tstrsplit(id, split = "_")
][, id := NULL]

if ("tdirp" %in% ls()) {
  dir <-
    fs::path_dir(tdirp) |>
    fs::path("stata")

  haven::write_dta(rd, fs::path(dir, "gd_10points.dta"))

}
