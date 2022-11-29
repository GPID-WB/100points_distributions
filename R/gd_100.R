library(wbpip)



ppp_year <- 2017
nq       <- 10

pfw <- pipload::pip_load_aux("pfw") |>
  {\(.) .[use_bin == 1]}()


fpf <- pfw[, .(country_code,
               surveyid_year,
               reporting_year,
               welfare_type
)]

fpf[,
    id := paste(country_code, reporting_year, welfare_type, sep = "_")
]

# fpf <- fpf[1:2] |>
fpf <- fpf |>
  split(by = "id")


get_bin_dist <- function(pl) {

  dt   <- pipload::pip_load_cache(pl$country_code,
                                  pl$surveyid_year,
                                  verbose = FALSE)

  setorder(dt, welfare_ppp, weight)
  dt[,
     # get bins and total pop and welfare
     `:=`(
       bin = wbpip:::md_compute_bins(welfare_ppp,
                                     weight,
                                     nbins = nq,
                                     output = "simple"),
       tot_pop = sum(weight),
       tot_wlf = sum(welfare_ppp*weight)
     )
  ]

  dt[,
     # get avg wlf, pop and wlf shared
     c("avg_welfare", "pop_share", "welfare_share", "quantile") := {
       avg_welfare   <-  weighted.mean(welfare_ppp, weight)
       pop_share     <- sum(weight)/tot_pop
       welfare_share <- sum(welfare_ppp*weight)/tot_wlf
       quantile      <- max(welfare_ppp)
       list(avg_welfare, pop_share, welfare_share, quantile)
     },
     by = bin]

  dt <-
    dt[,
       lapply(.SD, mean),
       by = bin,
       .SDcols =  c("avg_welfare", "pop_share", "welfare_share", "quantile")
    ]

  dt[,
     `:=`(
       country_code = pl$country_code,
       year         = pl$reporting_year,
       welfare_type = pl$welfare_type
     )]

  return(dt)
}

poss_get_bin_dist <- purrr::possibly(.f = get_bin_dist,
                                     otherwise = NULL)
dr <- purrr::map(.x = fpf,
                 .f = poss_get_bin_dist)

# Problematic databases
dr_err <-
  dr |>
  purrr::keep(is.null) |>
  names()
dr_err

# Get rid of problematic data
dr <- purrr::compact(dr) |>
  rbindlist(use.names = TRUE)


if ("tdirp" %in% ls()) {
  dir <-
    fs::path_dir(tdirp) |>
    fs::path("stata")

  haven::write_dta(dr, fs::path(dir, "bin_10points.dta"))

}
