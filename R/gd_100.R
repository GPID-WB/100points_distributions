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
library(wbpip)

# pipfun::pipinstall("pipfun", "ongoing", dependencies = FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                   Subfunctions   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

gd_pop_wlf <- function(pl) {
  # pl <- as.list(environment())
  # pl <- list(...)
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


get_gd_calcs <- function(level, vctr, mean, id) {

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

poss_get_gd_calcs <- purrr::possibly(.f = get_gd_calcs,
                                     otherwise = NULL)

fmt_sve <- function(dt) {
  dt <- copy(dt)
  nvars <- c("country_code", "year", "welfare_type")
  id    <- unique(dt[, id])
  dt[,
     (nvars) := tstrsplit(id, split = "_")
  ][, id := NULL]

  # save
  haven::write_dta(dt, fs::path("data/singles", id, ext = "dta"))
  qs::qsave(dt, fs::path("data/singles", id, ext = "qs"))

}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                   Initial parameters   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ppp_year <- py <- 2017
nq       <- 100
lorenz   <- NULL
popshare <- seq(from = 1/nq, to = 1, by = 1/nq)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# load Aux data   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aux <- pipfun::pip_merge_aux(c("cpi", "ppp", "gdm"))

pfw <- pipload::pip_load_aux("pfw") |>
  {\(.) .[use_groupdata == 1 &
            inpovcal  == 1]}()

aux_id <- attr(aux, "id")
pfw_id <- pipfun::aux_ids("pfw")$pfw

byv <- intersect(aux_id, pfw_id)

# filter according to pfw
dt <- merge(aux, pfw, byv)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# deflate mean   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dt[, mean_ppp :=  {
      x <- deflate_welfare_mean(survey_mean_lcu,
                                ppp,
                                cpi)
      x <- x/(365/12)
    }
   ][,
      id := paste(country_code, surveyid_year, welfare_type, sep = "_")
    ]

mean_ppp <-
  dt[!is.na(mean_ppp)
  ][,  # keep no na data
    # keep important variables
    c("id", "mean_ppp", "data_level")
  ] |>
  # create list of means and reporting level by id
  split(by = "id",
        keep.by = FALSE) |>
  # convert data.table into vectors of means with reporting levels as names
  purrr::map(~{
    y        <- .x[, mean_ppp]
    names(y) <- .x[, data_level]
    attr(y,"label") <- NULL
    y
  })



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get population and welfare vctrs for Group data   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fpf <- pfw[, .(country_code,
               year,
               surveyid_year,
               reporting_year,
               survey_year,
               pop_domain,
               welfare_type)]

fpf[,
    id := paste(country_code, surveyid_year, welfare_type, sep = "_")
]

# fpf <- fpf[1:5]
# lf <- as.list(fpf)
pl <- split(fpf, by = "id")

vctrs <- purrr::map(pl, gd_pop_wlf)
names(vctrs) <- fpf[, id]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get distributions   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rd <-
  purrr::map(.x = names(vctrs),
             .f = ~{
               id <- .x
               y <- mean_ppp[[id]]
               v <- vctrs[[id]]

               levels <- names(y)
               purrr::map_df(.x = levels,
                             .f = poss_get_gd_calcs,
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# format and save data   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

purrr::walk(rd, fmt_sve)

# rd <- rbindlist(rd, use.names = TRUE)
