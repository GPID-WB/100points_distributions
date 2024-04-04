library(data.table)
library(tidyverse)

pl <- 2.15
pip <- pipr::get_stats(povline = pl)
setDT(pip)

x <- pipload::pip_load_cache(cache_id = "IND_2016_CPHS_D2_CON_GPWG")
x[,
  `:=`(
      reporting_level = area,
      welfare_lnp     = welfare_ppp,
      year            = 2016,
      n_imp           = uniqueN(imputation_id)
    )
  ][,
    frq_weight := weight
  ][,
    weight := frq_weight / n_imp
  ]



sdt <- x[,
           c("welfare_lnp",
             "weight",
             "reporting_level",
             "area",
             "survey_year",
             "frq_weight",
             "imputation_id"
             # ,
             # "dist_weight",
           )]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## saving --------

tempdir <- fs::path("p:/03.pip/estimates/lineup")
path <- tempdir |>
  fs::path("IND_2016_survey_year")

path |>
  fs::path(ext = "dta") |>
  haven::write_dta(data = sdt, path = _)

path |>
  fs::path(ext = "fst") |>
  fst::write_fst(x = sdt, path = _)



pl <- 2.15


sdt |>
  mutate(poor = welfare_lnp < pl) |>
  summarise(across(.cols = c(welfare_lnp, poor), # of these two variables
                   .fns = \(.) weighted.mean(., weight)
            ), # using this function

            # By all imputation ID
            .by = c("reporting_level",
                    "survey_year")
  )

pip |>
  # as_tibble() |>  # assume it is a tibble and not a data.table
  filter(country_code == "IND" & year == 2016) |>
  select(c(reporting_level, mean, headcount))


sdt[,
   # get bins and total pop and welfare
   `:=`(
     bin = wbpip:::md_compute_bins(welfare = welfare_lnp,
                                   weight =  weight,
                                   nbins = 10,
                                   output = "simple")[,bins],
     tot_pop = sum(weight),
     tot_wlf = sum(welfare_lnp*weight)
   ),
   by = c("reporting_level")]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## main measures --------

urru <-
  sdt[,
   # get avg wlf, pop and wlf shared
   .(
     avg_welfare   = weighted.mean(welfare_lnp, weight),
     pop_share     = sum(weight)/first(tot_pop) ,
     welfare_share = sum(welfare_lnp*weight)/first(tot_wlf),
     quantile      = max(welfare_lnp)
   ),
   by = c("reporting_level", "bin") ]


dcast(urru, bin ~ reporting_level, value.var = "welfare_share")

pip |>
  # as_tibble() |>  # assume it is a tibble and not a data.table
  filter(country_code == "IND" & year == 2016) |>
  select(contains("decile"), reporting_level)  |>
  pivot_longer(cols = starts_with("decile")) |>
  pivot_wider(names_from = reporting_level, values_from = value)


