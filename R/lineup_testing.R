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


pipload::pip_load_all_aux(aux = c("cpi", "ppp", "pfw", "pop", "countries", "gdp", "pce"),
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
  # inpovcal obs
  filter(inpovcal == 1) |>

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
                reportvar = FALSE) |>
  joyn::merge(countries[, c("country_code", "region_code")],
              by = "country_code",
              reportvar = FALSE,
              keep = "left")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Adjust NAC to survey years (for decimal years) --------

nac_adjusted <-
  map(.x = c("pop", "gdp", "pce"),
      .f = ~ {
        y <- .x
        vals <- map2_dbl(dt$survey_year, dt$data,
                 adjust_aux_values,
                 value_var = y)
        var_name <- paste0("svy_", y)
        tibble(!!var_name := vals)
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
                                  .default = welfare_type),
         lineup_year = as.numeric(surveyid_year)) |>
  select(!!nvars, cache_id, survey_id, lineup_year )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# reference table   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
eco_vars <- c("country_code",
              "welfare_type")

economy <-  du[, ..eco_vars ] |>
  unique()

rt <-
  expand_grid(economy, lineup_year = gls$PIP_REF_YEARS)


# find those with matching obs in cache_inv
rt_cach <-
joyn::merge(rt, cache_inv,
      by = c("country_code", "welfare_type", "lineup_year"))

svy_dt <- rt_cach[report == "x & y"][,
                                     c("country_code",
                                       "welfare_type",
                                       "lineup_year",
                                       "surveyid_year",
                                       "cache_id")] |>
  arrange(country_code, welfare_type)

lnp_dt <- rt_cach[report == "x"
                  ][,
                     c("country_code",
                         "welfare_type",
                         "lineup_year")
                    ][,
                      # Nest to isolate lineup year
                      .(lnp_data = .(.SD)),
                      keyby = c("country_code",
                                "welfare_type")
                      ] |>
  arrange(country_code, welfare_type)


# Nest cache inventory
cache_nest <-
  cache_inv[, .(cache_data = .(.SD)),
            keyby = c("country_code",
                      "welfare_type")
  ] |>
  semi_join(lnp_dt) |>
  arrange(country_code, welfare_type)



# Then match those that need lineup.

get_cacheid <- function(x, y) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # computations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  x <- x[, lineup_year]
  ly <- y[, lineup_year]
  ci <- y[, cache_id]
  si <- y[, surveyid_year]

  a = findInterval(x, ly)
  b <- a + 1
  a[a == 0] <- 1
  b[b > length(ly)] <- length(ly)

  z <- data.table(cache_id1 = ci[a],
                  cache_id2 = ci[b],
                  svy_year1 = si[a],
                  svy_year2 = si[b])
  z[,
    cache_id := list(.(unique(c(cache_id1,cache_id2)))),
    1:nrow(z)
    ][,
      surveyid_year := list(.(unique(c(svy_year1,svy_year2)))),
      1:nrow(z)]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # z[, c("surveyid_year", "cache_id")]
  # z[, c( "cache_id")]
  return(z)

}

de <-
  map2(.x = lnp_dt$lnp_data,
     .y = cache_nest$cache_data,
     .f = get_cacheid) |>
  list_rbind()


# unnest
df <- lnp_dt[seq_len(.N),
             lnp_data[[1L]],
             by = c("country_code", "welfare_type")
             ] # if I wanted to add the rest, nest with this `[lnp_dt, on = others]`

dg <- data.table(df, de)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# add nac to lineup years and nac survey to svy_yearx   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




