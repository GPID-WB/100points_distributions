library(dtplyr)
library(data.table)
library(tidyverse)

source("R/lineup/db_utils.R")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Initial conditions   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ct <- "IND"
yr <- 2015
yr <- 2020
yr <- 2014
pl <- 2.15


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load means data and metadata   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (!exists(x = "du")) source("R/lineup/svy_nac.R")

if (!exists(x = "memload")) memload <- memoise::memoise(pipload::pip_load_cache)

# This data is suuposed to be obtained within the pipeline, not afterwards

dt_ref_mean_pred <- qs::qread("data/dt_ref_mean_pred.qs")

pip <- pipr::get_stats(fill_gaps = TRUE, povline = pl)
setDT(pip)


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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Interpolation and extrapolation  ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Is interpolated? --------

interpolate <- is_to_interpolate(mtdt$survey_year)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## is same direction? --------


same_direction <- FALSE
if (interpolate) {
  same_direction <- is_same_direction(svy_means = mtdt$survey_mean_ppp,
                                      svy_nac   = proxy$svy_value,
                                      ref_nac   = proxy$ref_value)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## get growth factor --------

# if there is at least one interpolation of same direction
if (any(same_direction)) {
  gfs <- same_dir_growth(svy_means = mtdt$survey_mean_ppp,
                         svy_nac   = proxy$svy_value,
                         ref_nac   = proxy$ref_value)
}

# if there is at least one interpolation of diverging direction or if it
# extrapolation

if (!all(same_direction)) {
  # if it is extrapolated or interpolation of different directions
  gfd <- vector(mode = "numeric",
                length = length(proxy$svy_value))

  for (i in seq_along(proxy$svy_value)) {
    gfd[i] <- proxy$ref_value/proxy$svy_value[[i]]
  }
}

gf <- vector(mode = "numeric",
            length = length(proxy$svy_value))

wd <- which(same_direction == TRUE)

a <- c(1, 2)
for (i in seq_along(same_direction)) {
  gf[a] <-
    if (i %in% wd) {
      gfs[a]
    } else {
      gfd[a]
    }
  a <- a + 2
}

mtdt$growth_factor <- gf

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## distance weights --------

mtdt$distance_weight <- rep(1, times = length(mtdt$survey_year))
if (interpolate) {
  mtdt$distance_weight <- distance_weight(ref_year =  mtdt$reporting_year,
                                          svy_years = mtdt$survey_year)
}



# population in reference year
ref_pop <- unique(mtdt$reporting_pop)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# load and treat data   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# load data
ld <- vector(mode = "list",
             length = length(mtdt$survey_year))



for (i in seq_along(mtdt$cache_id)) {
  # NOTE: I should optimize the loading of the data so it is done only once.
  x <-  memload(cache_id = mtdt$cache_id[[i]])
  setkey(x, reporting_level)

  # get synthetic vector for group data.
  if (mtdt$distribution_type[[i]] %in% c("group", "aggregate")) {
    x <- wbpip:::sd_create_synth_vector(
      welfare    = x[reporting_level == mtdt$reporting_level[[i]],
                     welfare],# this is cumulative (not nominal values)
      population = x[reporting_level == mtdt$reporting_level[[i]],
                     weight],
      mean       = mtdt$survey_mean_ppp[[i]],
      pop        = mtdt$svy_pop[[i]]
    )

    x[, `:=`(
      area            = mtdt$reporting_level[[i]],
      reporting_level = mtdt$reporting_level[[i]],
      survey_year     = mtdt$survey_year[[i]]
      )]
    setnames(x, "welfare", "welfare_ppp")
  } else {
    x <- x[.(mtdt$reporting_level[[i]])] # filter by reporting level
  }

  x[,
    # Create lineup aggregate
    welfare_lnp := welfare_ppp * mtdt$growth_factor[[i]]
  ][,
    # Poor status
    poor := welfare_lnp < pl
  ]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## adjust to population of reference year --------

  # NOTE: I take the first observation of the sum of the weights by
  # imputation_id, assuming that each imputation expand to the same level of
  # population. I think it should always be the case, but if there is such a
  # case where each imputation expand to its own population, the procedure below
  # should be done by imputation.

  svy_pop <- x[,
               sum(weight, na.rm = TRUE),
               by = "imputation_id"][1, V1]
  x[,
    # adjust to WDI population
    weight := weight * (mtdt$reporting_pop[[i]]/svy_pop)
  ][,
    # adjust by distance
    weight := weight * mtdt$distance_weight[[i]]
  ]

  ld[[i]] <- x
}


sdt <- rbindlist(ld, use.names = TRUE, fill = TRUE)
setkeyv(sdt, c("reporting_level", "survey_year", "imputation_id"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# comparison with PIP   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

waldo::compare(
  y = mtdt$predicted_mean_ppp,
  x = sdt[,
          stats::weighted.mean(welfare_lnp, weight),
          keyby = c("reporting_level", "survey_year")
          ][, V1],
  tolerance = 1e-12
)


sdt[,
    stats::weighted.mean(poor, weight),
    by = c("reporting_level", "survey_year", "imputation_id")
    ][, mean(V1),
      keyby = c("reporting_level", "survey_year")
      ][,
        nw := mtdt$distance_weight
        ][, stats::weighted.mean(V1, nw),
          keyby = c("reporting_level")
          ]


sdt[,
    stats::weighted.mean(poor, weight),
    by = "reporting_level"][, V1]



pov <- sdt[,
           stats::weighted.mean(poor, weight),
           by = "reporting_level"][, V1] |>
  c(sdt[,
        stats::weighted.mean(poor, weight)]) |>
  unique()

pip[country_code == ct & year == yr, headcount, by = "reporting_level"]

waldo::compare(
  pip[country_code == ct & year == yr, headcount],
  pov,
  tolerance = 1e-12
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# save    ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  keep important variables --------

sdt <- sdt[,
           c("welfare_lnp", "weight", "reporting_level",  "area", "survey_year")
][,
  year := yr]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## file name --------


### file ---------


if (interpolate) {
  sme <- NULL
  for (i in seq_along(same_direction)) {
    if (same_direction[[i]] == TRUE) {
      sme <- paste(sme, "int_same", sep = "_")
    } else {
      sme <- paste(sme, "int_diverging", sep = "_")
    }
  }
} else {
  sme <- "_ext"
}



file_name <-
  paste(ct,yr, sep = "_") |>
  paste0(sme)
file_name


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## saving --------


path <- tdirp |>
  fs::path(file_name)

path |>
  fs::path(ext = "dta") |>
  haven::write_dta(data = sdt, path = _)

path |>
  fs::path(ext = "fst") |>
  fst::write_fst(x = sdt, path = _)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# END of SCRIPT   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




#
#
pce |>
  filter(country_code == "CHN") |>
  ggplot() +
  geom_line(aes(x = year,
                y = pce))


pip |>
  filter(country_code == "CHN", year > 2000) |>
  mutate(nac = hfce / 365) |>
  ggplot() +
  geom_line(aes(x = year,
                y = nac)) +
  geom_line(aes(x = year,
                y = mean,
                color = reporting_level))




#





# dta_file <-
#   fs::dir_ls("data/test") |>
#   fs::path_ext_remove()
#
# dta_file |>
#   fs::path(ext = "dta") |>
#   map(haven::read_dta) |>
#   walk2(fs::path(dta_file, ext = "fst"), fst::write_fst)
#
#
# fs::dir_copy("data/test", tdirp, overwrite = TRUE)
