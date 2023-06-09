library(dtplyr)
library(data.table)
library(tidyverse)
source("R/db_utils.R")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load Globas   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
py <- 2017 # PPP YEar
pattern <- paste0("_", py,"_.+PROD$")


version <-
  # get survey directory
  pipfun::pip_create_globals(
    root_dir   = Sys.getenv("PIP_ROOT_DIR"),
    create_dir = FALSE)$PIP_PIPE_DIR |>
    fs::path('pc_data/cache/clean_survey_data') |>
  # extract versions
    fs::dir_ls(type = "dir") |>
    fs::path_file() |>
  # filter by ppp year
    grep(pattern, x = _, value = TRUE) |>
  #get most recent
    max()


gls <- pipfun::pip_create_globals(
  root_dir   = Sys.getenv("PIP_ROOT_DIR"),
  vintage = version,
  create_dir = FALSE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load data and create NAC file   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Merge PFW info with NAC   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


lpfw <- lazy_dt(pfw)


dom_vars <- str_subset(names(pfw), "domain$")
dt <-
  pfw |>
    # get max doming per obs
    # rowwise() |>  # this could be faster with data.table or reshaping
    # mutate(domain = max(c_across(ends_with("domain")),
    #                     na.rm = TRUE)) |>
  copy() |>
  {\(.) .[,
          domain := apply(.SD, 1, max, na.rm = TRUE),
          .SDcols = dom_vars]}() |>

  # Expand for those with double wlefare in the same survey
  mutate(exp_4welf = if_else(oth_welfare1_type == "", 1, 2)) |>
  uncount(exp_4welf,
          .id = "add_welf") |>
  mutate(oth_welfare1_type = if_else(add_welf == 1,
                                     "",
                                     oth_welfare1_type),
         welfare_type = case_when(
    grepl("^c", tolower(oth_welfare1_type)) ~ "consumption",
    grepl("^i", tolower(oth_welfare1_type)) ~ "income",
    .default = welfare_type
    )) |>

  # select vars
  select(country_code,
         survey_acronym,
         contains("year"),
         welfare_type,
         domain) |>

  # duplicates for urban/rural
  expand_grid(data_level = c("national", "urban", "rural")) |>
  # merge with nac
    joyn::merge(nac,
                by = c("country_code", "data_level"),
                match_type = "m:1",
                keep = "inner",
                reportvar = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Adjust NAC to survey years (for decimal years) --------

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
  select(!c(data, ref_year_des)) |>
  filter(data_level == "national" & domain == 1 |
           data_level != "national" & domain == 2 )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# get surveys IDs  ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cache_pattern <- "([[:upper:]]{3})_(\\d{4})_([^_]+)_(D[12])_(CON|INC)_(.+)$"
nvars <-
  c(
    "country_code",
    "surveyid_year",
    "survey_acronym",
    "data_level",
    "welfare_type",
    "source"
  )

cache_inv <-
  pipload::pip_load_cache_inventory(version = version) |>
  # takenize cache_id
  copy() |>
  {\(.) .[,
          (nvars) := tstrsplit(x = cache_id,
                               split = "_")]}() |>
  mutate(welfare_type = case_when(grepl("^c", tolower(welfare_type)) ~ "consumption",
                                  grepl("^i", tolower(welfare_type)) ~ "income",
                                  .default = welfare_type)) |>
  select(!!nvars, cache_id, survey_id )





df <- pipload::pip_load_cache(cache_id = "AGO_2000_HBS_D1_CON_GPWG")
