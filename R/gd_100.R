# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# project:       Create 100-point distribution for group data
# Author:        Andres Castaneda
# Dependencies:  The World Bank
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Creation Date:    2022-12-06
# Modification Date:
# Script version:    01
# References:
#
#
# Output:             data.table with distribution
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run initial conditions   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("R/init.R")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# load Aux data   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aux <- pipfun::pip_merge_aux(c("cpi", "ppp", "gdm"))

pfw <- pipload::pip_load_aux("pfw") |>
  {\(.) .[use_groupdata == 1 &
            inpovcal  == 1]}()

aux_id <- attr(aux, "id")
pfw_id <- pipfun::aux_ids("pfw")$pfw

byv <- intersect(aux_id, pfw_id)

# filter according to pfw
dt <- merge(aux, pfw, byv)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# deflate mean   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dt[, mean_ppp :=  {
      x <- deflate_welfare_mean(survey_mean_lcu,
                                ppp,
                                cpi)
      x <- x/(365/12)
    }
   ][,
      id := paste(country_code, surveyid_year, welfare_type, sep = "_")
    ]

mean_ppp <-
  dt[!is.na(mean_ppp)
  ][,  # keep no na data
    # keep important variables
    c("id", "mean_ppp", "data_level")
  ] |>
  # create list of means and reporting level by id
  split(by = "id",
        keep.by = FALSE) |>
  # convert data.table into vectors of means with reporting levels as names
  map(~{
    y        <- .x[, mean_ppp]
    names(y) <- .x[, data_level]
    attr(y,"label") <- NULL
    y
  })



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get population and welfare vctrs for Group data   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fpf <- pfw[, .(country_code,
               year,
               surveyid_year,
               reporting_year,
               survey_year,
               pop_domain,
               welfare_type)]

fpf[,
    id := paste(country_code, surveyid_year, welfare_type, sep = "_")
]

# fpf <- fpf[1:5]
# lf <- as.list(fpf)
pl <- split(fpf, by = "id")

vctrs <- map(pl, gd_pop_wlf)
names(vctrs) <- fpf[, id]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get distributions   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rd <-
  map(.x = names(vctrs),
             .f = ~{
               id <- .x
               y <- mean_ppp[[id]]
               v <- vctrs[[id]]

               levels <- names(y)
               map_df(.x = levels,
                             .f = poss_get_gd_calcs,
                             vctr = v,
                             mean = y,
                             id   = id)
             })

# Problematic databases
rd_err <-
  rd |>
  keep(is.null) |>
  names()
rd_err

# Get rid of problematic data
rd <- compact(rd)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# format and save data   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

walk(rd, fmt_sve)

# rd <- rbindlist(rd, use.names = TRUE)
