library(dtplyr)
library(data.table)
library(tidyverse)
source("R/db_utils.R")

pipload::pip_load_all_aux(aux = c("cpi", "ppp", "pfw", "pop", "gdm", "gdp", "pce"),
                          replace = TRUE)

# merge pop, gdp, and cpe
nac <-
  joyn::merge(gdp, pce,
             by = c("country_code", "year", "gdp_data_level=pce_data_level"),
             reportvar = FALSE) |>
  joyn::merge(pop,
              by = c("country_code", "year", "gdp_data_level=pop_data_level"),
              reportvar = FALSE) |>
  # rename to data_level for all
  rename(data_level = gdp_data_level) |>
  # remove domain varaibles
  select(!ends_with("domain")) |>
  group_by(country_code, data_level) |>
  group_nest()


lpfw <- lazy_dt(pfw)
dt <-
  pfw |>
    # get max doming per obs
    rowwise() |>  # this could be faster with data.table or reshaping
    mutate(domain = max(c_across(ends_with("domain")),
                        na.rm = TRUE)) |>
    # select vars
    select(country_code, survey_acronym, contains("year"), domain) |>
    # duplicates for urban/rural
    expand_grid(data_level = c("national", "urban", "rural")) |>
  # merge with nac
    joyn::merge(nac,
                by = c("country_code", "data_level"),
                match_type = "m:1",
                keep = "inner",
                reportvar = FALSE)




nac_adjusted <-
  map(.x = c("pop", "gdp", "pce"),
      .f = ~ {
        y <- .x
        vals <- map2_dbl(dt$survey_year, dt$data,
                 adjust_aux_values,
                 value_var = y)
          tibble(!!y := vals)
      }) |>
  list_cbind()

# join
du <-
  bind_cols(dt, nac_adjusted) |>
  select(!data)




