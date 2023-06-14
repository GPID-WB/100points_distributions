library(dtplyr)
library(data.table)
library(tidyverse)
source("R/lineup/db_utils.R")
source("R/lineup/svy_nac.R")


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




