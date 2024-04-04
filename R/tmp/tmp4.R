library(data.table)
library(tidyverse)
library(collapse)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# load data   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pl <- 2.15
path <- fs::path("p:/03.pip/estimates/lineup/IND_2016_survey_year.fst")
sdt  <-  fst::read_fst(path = path)

pip <- pipr::get_stats(povline = pl)
setDT(pip)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# poverty ane mean   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## tidyverse way --------

sdt |>
  mutate(poor = welfare_lnp < pl) |>
  summarise(across(.cols = c(welfare_lnp, poor), # of these two variables
                   .fns = \(.) weighted.mean(., weight)
  ), # using this function

  # By all imputation ID
  .by = c("reporting_level",
          "survey_year")
  )


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## collapse way --------

sdt |>
  ftransform(poor = welfare_lnp < pl) |>
  fgroup_by(reporting_level, survey_year) |>
  # get_vars(c("welfare_lnp", "poor")) |>
  fselect(welfare_lnp, poor, weight) |>
  fmean(weight)


pip |>
  # as_tibble() |>  # assume it is a tibble and not a data.table
  filter(country_code == "IND" & year == 2016) |>
  select(c(reporting_level, mean, headcount))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# deciles   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



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
  pivot_wider(names_from = reporting_level, values_from = value) |>
  as.data.table()

