library(dtplyr)
library(data.table)
library(tidyverse)

source("R/lineup/db_utils.R")

if (!exists(x = "du")) source("R/lineup/svy_nac.R")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load means data   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This data is suuposed to be obtained within the pipeline, not afterwards

dt_ref_mean_pred <- qs::qread("data/dt_ref_mean_pred.qs")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# extrapolation   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ct <- "AGO"
yr    <- 1990

# Filtered lineup mean

flm <- filter2list(dt_ref_mean_pred, country_code == ct & reporting_year == yr)

# fileteres svy nac data

fnac <- filter2list(du, country_code == ct & surveyid_year %in% flm$surveyid_year)

# append lists with all metadata
mtdt <- append(flm, fnac)




# load data

# sdt <- pmap(.l = mtdt,
#             .f = \(...) {
#               l <- list(...)
#               x <- pipload::pip_load_cache(cache_id = l$cache_id)
#             }
#             )


sdt <- pipload::pip_load_cache(cache_id = mtdt$cache_id)

proxy <- select_proxy(survey_gdp  = mtdt$svy_gdp,
                      survey_pce  = mtdt$svy_pce,
                      ref_gdp     = mtdt$reporting_gdp,
                      ref_pce     = mtdt$reporting_pce,
                      region_code = mtdt$region_code)


sdt[, welfare_lnp := welfare_ppp * (proxy$ref_value/proxy$svy_value1) ]

waldo::compare(
  x = sdt[, stats::weighted.mean(welfare_lnp, weight)],
  y = mtdt$predicted_mean_ppp,
  tolerance = 1e-12
)




# gdp |>
#   filter(country_code == "COL") |>
#   ggplot() +
#   geom_line(aes(x = year,
#                 y = gdp))
#
#
#


