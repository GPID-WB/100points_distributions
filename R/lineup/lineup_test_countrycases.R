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
# Interpolation adn extrapolation  ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


ct <- "AGO"
yr <- 1990
yr <- 2008
yr <- 2004
pl <- 2.15

# Filtered lineup mean

flm <- filter2list(dt_ref_mean_pred, country_code == ct & reporting_year == yr)

# fileteres svy nac data

fnac <- filter2list(du, country_code == ct & surveyid_year %in% flm$surveyid_year)

# append lists with all metadata
mtdt <- append(flm, fnac)


proxy <- select_proxy(survey_gdp  = mtdt$svy_gdp,
                      survey_pce  = mtdt$svy_pce,
                      ref_gdp     = mtdt$reporting_gdp,
                      ref_pce     = mtdt$reporting_pce,
                      region_code = mtdt$region_code)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## find out if it is an interpolated or extrapolated case --------


interpolate <- is_to_interpolate(mtdt$survey_year)

same_direction <- FALSE
if (interpolate) {
  same_direction <- is_same_direction_interpolated(
    survey_mean1 = mtdt$survey_mean_ppp[[1]],
    survey_mean2 = mtdt$survey_mean_ppp[[2]],
    svy_value1   = proxy$svy_value[[1]],
    svy_value2   = proxy$svy_value[[2]],
    ref_value    =  proxy$ref_value)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## get growth factor --------

# if it is interpolation of same direction
if (same_direction) {
  # estimate mean of reference year
  ref_mean <- est_ref_mean(
    survey_mean1 = mtdt$survey_mean_ppp[[1]],
    survey_mean2 = mtdt$survey_mean_ppp[[2]],
    svy_value1   = proxy$svy_value[[1]],
    svy_value2   = proxy$svy_value[[2]],
    ref_value    = proxy$ref_value)

  # growth factors
  g1 <- ref_mean/mtdt$survey_mean_ppp[[1]]
  g2 <- ref_mean/mtdt$survey_mean_ppp[[2]]

  mtdt$growth_factor <- c(g1, g2)

} else {
  # if it is extrapolated or interpolation of different directions
  gf <- vector(mode = "numeric",
               length = length(proxy$svy_value))

  for (i in seq_along(proxy$svy_value)) {
    gf[i] <- proxy$ref_value/proxy$svy_value[[i]]
  }

  mtdt$growth_factor <- gf
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## distance weights --------

mtdt$distance_weight <- distance_weight(ref_year =  mtdt$reporting_year,
                                        svy_years = mtdt$survey_year)

# population in reference year
ref_pop <- unique(mtdt$reporting_pop)

# load data
ld <- vector(mode = "list",
             length = length(mtdt$survey_year))

for (i in seq_along(mtdt$survey_year)) {
  x <-  pipload::pip_load_cache(cache_id = mtdt$cache_id[[i]])

  x[,
    # Create lineup aggregate
    welfare_lnp := welfare_ppp * mtdt$growth_factor[[i]]
  ][,
    # Poor status
    poor := welfare_lnp < pl
    ]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## adjust to population of reference year --------
  svy_pop <- x[, sum(weight, na.rm = TRUE)]
  x[,
    # adjust to WDI population
    weight := weight * (ref_pop/svy_pop)
    ][,
      # adjust by distance
      weight := weight * mtdt$distance_weight[[i]]
      ]

  ld[[i]] <- x
}


sdt <- rbindlist(ld, use.names = TRUE, fill = TRUE)



waldo::compare(
  x = sdt[,
          stats::weighted.mean(welfare_lnp, weight),
          by = "cache_id"][, V1],
  y = mtdt$predicted_mean_ppp,
  tolerance = 1e-12
)



pip <- pipr::get_stats(ct, yr, fill_gaps = TRUE)

waldo::compare(
  sdt[, stats::weighted.mean(poor, weight)],
  pip$headcount,
  tolerance = 1e-12
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# save and keep important variables   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

sdt <- sdt[,
           c("welfare_lnp", "weight", "area", "survey_year")
           ][,
             year := yr]



if (interpolate) {

  if (same_direction)  {
    sme <- "int_same"
  } else {
    sme <- "int_diverging"
  }

} else {
  sme <- "ext"
}

file_name <- paste(ct,yr, sme, sep = "_")

path <- "data/test" |>
  fs::path(file_name, ext = "dta")

haven::write_dta(sdt, path)


fs::dir_copy("data/test", tdirp, overwrite = TRUE)




# gdp |>
#   filter(country_code == "COL") |>
#   ggplot() +
#   geom_line(aes(x = year,
#                 y = gdp))
#
#
#


