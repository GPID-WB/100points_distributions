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

  id <- glue("{id}_{nq}bin_{ppp_year}PPP")

  # save
  # haven::write_dta(dt, fs::path("data/singles", id, ext = "dta"))
  qs::qsave(dt, fs::path("data/singles", id, ext = "qs"))

}




get_micro_dist <- function(pl) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## load data and order --------

  dt   <- pipload::pip_load_cache(pl$country_code,
                                  pl$surveyid_year,
                                  verbose = FALSE)

  setorder(dt, imputation_id, reporting_level, welfare_ppp, weight)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## bins ant totals --------

  dt[,
     # get bins and total pop and welfare
     `:=`(
       bin = wbpip:::md_compute_bins(welfare_ppp,
                                     weight,
                                     nbins = nq,
                                     output = "simple"),
       tot_pop = sum(weight),
       tot_wlf = sum(welfare_ppp*weight)
     ),
     by = c("imputation_id", "reporting_level")
  ]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## main measures --------

  dt[,
     # get avg wlf, pop and wlf shared
     c("avg_welfare", "pop_share", "welfare_share", "quantile") := {
       avg_welfare   <-  weighted.mean(welfare_ppp, weight)
       pop_share     <- sum(weight)/tot_pop
       welfare_share <- sum(welfare_ppp*weight)/tot_wlf
       quantile      <- max(welfare_ppp)
       list(avg_welfare, pop_share, welfare_share, quantile)
     },
     by = .(imputation_id, reporting_level, bin)]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Getting means --------

  dt <-
    dt[,
       # mean by impuration and bin
       lapply(.SD, mean),
       by = .(imputation_id, reporting_level, bin),
       .SDcols =  c("avg_welfare", "pop_share", "welfare_share", "quantile")
    ][,
      # mean by bin
      lapply(.SD, mean),
      by = .(reporting_level, bin),
      .SDcols =  c("avg_welfare", "pop_share", "welfare_share", "quantile")
    ]

  dt[,
     id := pl$id]

  return(dt)
}
