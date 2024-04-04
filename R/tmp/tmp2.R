#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# load packages   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# library(dtplyr)  # worth talking about this.
library(data.table)
library(tidyverse)

# If  you don't have installed some of the packages that I use below you, it is
# likely that Rstudio is going to let you know that you need to install them.
# Just click "yes"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# load data   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# pipr is not available in CRAN yet, but you can isntall it with the following
# pak::pak("worldbank/pipr")


pl  <- 2.15
pip <- pipr::get_stats(fill_gaps = TRUE, povline = pl)
setDT(pip) # convert to data.table

datadir <- fs::path("p:/03.pip/estimates/lineup")

datadir |>
  fs::dir_ls(regexp = "fst$")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Group data   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

chn <- fst::read_fst(fs::path(datadir, "CHN_2007_int_same_int_same.fst"),
                     as.data.table = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## using tidyverse --------

# estiamtes rur/urb mean
chn |>
  mutate(poor = welfare_lnp < pl) |>
  # get the weighted mean
  summarise(across(.cols = c(welfare_lnp, poor), # of these two variables
                   .fns = \(.) weighted.mean(., weight)), # using this function
            .by = reporting_level   # by repoting level
            ) |>
  # Append with national mean
  bind_rows(
    chn |>
      mutate(poor = welfare_lnp < pl) |>
      summarise(across(.cols = c(welfare_lnp, poor),
                       .fns = \(x) weighted.mean(x, weight))) |>
      mutate(reporting_level = "national")
  ) |>
  # rename
  rename(mean = welfare_lnp,
         headcount = poor)

# compare with pip
pip |>
  as_tibble() |>  # assume it is a tibble and not a data.table
  filter(country_code == "CHN" & year == 2007) |>
  select(c(reporting_level, mean, headcount)) |>
  mutate(across(-reporting_level , ~ num(.x, digits = 6)))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## using data.table --------


# estiamtes rur/urb mean
rururb <-
  chn[,
      poor := welfare_lnp < pl
      ][,
        lapply(.SD, weighted.mean, weight),
        .SDcol = c("welfare_lnp", "poor"),
        by = "reporting_level"]

# national
national <-
  chn[,
      poor := welfare_lnp < pl
      ][,
        lapply(.SD, weighted.mean, weight),
        .SDcol = c("welfare_lnp", "poor")
        ][,
        reporting_level := "national"]

# append
res <- rbindlist(list(rururb, national), use.names = TRUE)
setnames(x = res,
           old = c("welfare_lnp", "poor"),
           new = c("mean", "headcount"))
res[]

pip[country_code == "CHN" & year == 2007,
    .(reporting_level, mean, headcount)]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# imputed data   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# The process below is complex because it aims to take into account all possible
# cases, so it is better to have one complex code that manages all cases rather
# than having one code per case and having to modify them all in case something
# changes... that is always a trafeoff in programming


ind <- fst::read_fst(fs::path(datadir, "IND_2014_int_same_int_diverging.fst"),
                     as.data.table = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## using tidyverse --------
dt <-
  ind |>
  # get poverty rate per imputation id. NOtice that that if it is not imputed
  # data, it just gets the headcount for that particular survey
  mutate(poor = welfare_lnp < pl) |>
  summarise(weight2 = sum(weight), # get all the sum of weights
            across(.cols = c(welfare_lnp, poor), # of these two variables
                   .fns = \(.) weighted.mean(., weight)
            ), # using this function

            # By all imputation ID
            .by = c("reporting_level",
                    "survey_year",
                    "dist_weight",
                    "imputation_id")
  ) |>
  # get poverty rate across imputations. This does not require weighting.
  summarise(
    weight = dplyr::first(weight2),
    across(.cols = c(welfare_lnp, poor), mean), # using this function,
    .by = c(reporting_level, survey_year, dist_weight)
  ) |>
  # get poverty by reporting level
  summarise(weight = sum(weight),
            across(.cols = c(poor, welfare_lnp),
                   .fns = \(.) weighted.mean(., dist_weight)
            ),
            .by     = c("reporting_level")
  ) %>%
  # this is a good example of the use of the magritr pipe. Yet, It could be have
  # been done using another object
  bind_rows(.,
            . |>
              summarise(across(.cols = c(poor, welfare_lnp),
                               .fns = \(.) weighted.mean(., weight)
                               )
              ) |>
              mutate(reporting_level = "national")
  ) |>
  # Another way to rename, keep, and re order variables
  select(reporting_level,
         mean      = welfare_lnp,
         headcount = poor)

# compare with pip
pip |>
  as_tibble() |>  # assume it is a tibble and not a data.table
  filter(country_code == "IND" & year == 2014) |>
  select(c(reporting_level, mean, headcount)) |>
  mutate(across(-reporting_level , ~ num(.x, digits = 6)))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## using data.table --------

# estiamtes rur/urb mean
dt <-
  ind[,
      # generate poverty status
      poor := welfare_lnp < pl
      ][,
        # Weighted mean by imputaiton ID
        c(
          lapply(.SD, weighted.mean, weight),
          weight = sum(weight, na.rm = TRUE)
          ),
        .SDcol = c("welfare_lnp", "poor"),
        keyby = c("reporting_level",
                  "survey_year",
                  "dist_weight",
                  "imputation_id")
        ][,
          # get mean across imputation ID
          c(
            lapply(.SD, mean, na.rm = TRUE),
            weight = first(weight)
            ),
          .SDcol = c("welfare_lnp", "poor"),
          keyby = c("reporting_level", "survey_year", "dist_weight")
        ][,
          # get distance weighted estimates
          lapply(.SD, weighted.mean,dist_weight),
          .SDcol = c("weight", "welfare_lnp", "poor"),
          keyby = c("reporting_level")
        ]

dtn <-
  dt[,
     lapply(.SD, weighted.mean,weight),
     .SDcol = c("welfare_lnp", "poor")
     ][,
       reporting_level := "national"]

res <- rbindlist(list(dt, dtn), fill = TRUE)
res[,
    weight := NULL]
setnames(x = res,
         old = c("welfare_lnp", "poor"),
         new = c("mean", "headcount"))
res[]

pip[country_code == "IND" & year == 2014,
    .(reporting_level, mean, headcount)]



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# you can try with package collapse   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

