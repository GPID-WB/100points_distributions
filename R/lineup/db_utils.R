#' Adjust auxiliary data
#'
#' Adjust auxiliary statistics for surveys that span multiple calender years.
#'
#' GDP, PCE and other auxiliary statistics are available for calendar years.
#' Some surveys do however span over two calendar years. This function can be
#' used to adjust for the corresponding mismatch between calendar and survey
#' year.
#'
#' Values are adjusted by the weighted average of the years in question.
#'
#' @param svy_year numeric: A vector with the decimal survey year.
#' @param aux_df data.frame: A data frame containing the variable to be
#'   retrieved.
#' @param value_var character: The variable to be adjusted.
#'
#' @return numeric
#' @keywords internal
adjust_aux_values <- function(svy_year, aux_df, value_var) {
  years <- get_years(svy_year)
  weights <- get_weights(svy_year)
  values <- get_values(years, aux_df = aux_df, value_var = value_var)
  adjusted_value <- safe_weighted_mean(x = values, w = weights)
  return(adjusted_value)
}

#' Safe (purrr::possibly) variant of stats::weighted.mean
#'
#' @param x numeric: A vector with values whose weighted mean is to be computed.
#' @param w numeric: A vector with weights.
#'
#' @noRd
safe_weighted_mean <- purrr::possibly(function(x, w) {
  stats::weighted.mean(x, w)
}, otherwise = NA)

#' Get survey weights
#'
#' In case the survey year spans two calendar years this helper function returns
#' the proportion of the survey year in each respective calendar year.
#'
#' @param svy_year numeric: The survey year, including decimal.
#'
#' @return numeric
#' @noRd
get_weights <- function(svy_year) {
  if (svy_year %% 1 == 0) {
    return(1) # No need for weighted average for single years
  } else {
    weight2 <- svy_year %% 1
    weight1 <- 1 - weight2
    return(c(weight1, weight2))
  }
}

#' Get survey years
#'
#' In case the survey year spans two calendar years this helper function returns
#' the calendar year(s) when the survey was implemented.
#'
#' @param svy_year numeric: The survey year, including decimal.
#'
#' @return numeric
#' @noRd
get_years <- function(svy_year) {
  if (svy_year %% 1 == 0) {
    return(svy_year)
  } else {
    years <- floor(svy_year)
    years <- c(years, years + 1)
    return(years)
  }
}

#' Get values
#'
#' In case the survey year spans two calendar years this helper function
#' retrieves the relevant values for the years in question.
#'
#' @param years numeric: Year(s).
#' @param aux_df data.frame: A data frame containing the variable to be
#'   retrieved.
#' @param value_var character: The variable containing the value of interest.
#'
#' @return numeric
#' @noRd
get_values <- function(years, aux_df, value_var) {
  keep <- aux_df[["year"]] %in% years
  out <- aux_df[[value_var]][keep]
  return(out)
}




#' FIlter data.frame and convert it to list
#'
#' @param dt_ data.frame
#' @param condition condition in `i`  as in  `DT[i,j]`
#'
#' @return
#' @export
#'
#' @examples
#' library(data.table)
#' dt <- data.table(x = c("a", "a", "b", "c", "c"),
#'  y = 1:5)
#'
#' filter2list(dt, y > 3)
filter2list <- function(dt_, condition) {
  # change to data.table
  if (isFALSE(is.data.table(dt_))) {
    dt_ <- as.data.table(dt_)
  }

  zt <- dt_[eval(substitute(condition))] |>
    as.list()
  return(zt)

}





#' Select proxy values
#'
#' Select GDP or PCE as proxy value based on pre-specified rules.
#'
#' PCE is the default for all countries, except in Sub-Saharan Africa where GDP
#' is always used. If any PCE value is missing for the specific reference year,
#' GDP is used.
#'
#' @param region_code character: World Bank three letter region code.
#' @param ref_gdp numeric: GDP value for the reference year.
#' @param ref_pce numeric: PCE value for the reference year.
#' @param survey_gdp numeric: GDP value for the survey year.
#' @param survey_pce numeric: PCE value for the survey year.
#'
#' @return list
#' @references
#' Prydz, E.B., D. Jolliffe, C. Lakner, D.G. Mahler, P. Sangraula. 2019.
#' "[National Accounts Data used in Global Poverty Measurement](http://documents1.worldbank.org/curated/en/664751553100573765/pdf/135460-WP-PUBLIC-Disclosed-3-21-2019.pdf)".
#' Global Poverty Monitoring Technical Note 8.
#' World Bank, Washington, DC.
#' @keywords internal
select_proxy <- function(survey_gdp,
                         survey_pce,
                         ref_gdp,
                         ref_pce,
                         region_code) {

  # If Sub-Saharan Africa, OR
  # if other region but but PCE is NA, then use GDP

  if (unique(region_code) == "SSA" || anyNA(c(survey_pce, ref_pce)))  {
    proxy <- list(
      svy_value  = survey_gdp,
      ref_value  = unique(ref_gdp)
    )
  } else {
    proxy <- list(
      svy_value = survey_pce,
      ref_value  = unique(ref_pce)
    )
  }
  return(proxy)
}


#' get weights from survey year to reference year
#'
#' @param ref_year numeric: reference year
#' @param svy_years numeric: survey years. Length either 1 or 2
#'
#' @return numeric vector with both weights that sum up to 1
#' @export
distance_weight <- function(ref_year, svy_years) {

  # early return
  l_sy <- length(svy_years)
  if (l_sy == 1) {
    return(1)
  }

  # check reference year is unique
  ref_year  <- unique(ref_year)
  stopifnot(exprs = {
    length(ref_year) == 1
    l_sy %% 2 == 0
  })

  x <- matrix(svy_years, nrow = 2)

  wt <- vector(mode   = "numeric",
               length = l_sy)

  a <- c(1,2)
  for (i in seq_along(x[1, ])) {
    svy_year1 <- x[1, i]
    svy_year2 <- x[2, i]

    weight1 <- (svy_year2 - ref_year)/(svy_year2 - svy_year1)
    weight2 <- 1 - weight1
    wt[a] <- c(weight1, weight2)
    a <-  a + 2
  }

  return(wt)

}


#' Is growth from survyes the same of NAC
#'
#' checks growth in the survey mean between the two surveys is of the same sign
#' as (1) the growth in national accounts from the first survey to the reference
#' year, and (2) from the reference year to the second survey.
#'
#' @param svy_means numeric: vector of survey means
#' @param svy_nac   numeric: vector of National Accounts values
#' @param ref_nac   numeric: vector of national account value in reference year
#'
#' @return logical vector
#' @export
is_same_direction <- function(svy_means,
                              svy_nac,
                              ref_nac) {

  # early return
  l_sm <- length(svy_means)  # survey mean
  l_nv <- length(svy_nac)    # nac value
  if (l_sm == 1) {
    return(FALSE)
  }

  # check reference year is unique
  ref_nac  <- unique(ref_nac)
  stopifnot(exprs = {
    length(ref_nac) == 1
    length(l_nv) == length(l_sm)
    l_sm %% 2 == 0
  })

  x <- matrix(svy_means, nrow = 2)
  y <- matrix(svy_nac, nrow = 2)

  sdir <- vector(mode   = "logical",
               length = ncol(x))

  for (i in seq_along(x[1, ])) {
    sdir[i] <- is_growth_same_direction(survey_mean1 = x[1, i],
                                        survey_mean2 = x[2, i],
                                        svy_value1   = y[1, i],
                                        svy_value2   = y[2, i],
                                        ref_value    = ref_nac)
  }
  return(sdir)
}


#' Find whether svy growth and nac growth go in the same direction
#'
#' @param survey_mean1 numeric: value of survey mean in year 1
#' @param survey_mean2 numeric: value of survey mean in year 2
#' @param svy_value1 numeric: value of national accounts in survey year 1
#' @param svy_value2 numeric: value of national accounts in survey year 2
#' @param ref_value numeric: value  of national accounts in reference year
#'
#' @return logical
#' @export
is_growth_same_direction <- function(survey_mean1,
                                     survey_mean2,
                                     svy_value1,
                                     svy_value2,
                                     ref_value) {

  im <- is_monotonic(x1 = svy_value1, x2 = svy_value2, r = ref_value)

  if (im) {
    ism <- is_growth_same_sign(x = c(svy_value1, svy_value2),
                             y = c(survey_mean1, survey_mean2))
    if (ism) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}


#' is_monotonic makes sure that the reference year is in between the survey
#' years
#' @param x1 numeric: Value for the first year.
#' @param x2 numeric: Value for the second year.
#' @param r numeric: Value for the request year.
#' @return logical
#' @noRd
is_monotonic <- function(x1, x2, r) {
  ((r - x1) * (x2 - r)) > 0
}


#' is_growth_same_sign
#' @param x numeric: A vector with values to compare.
#' @param y numeric: A vector with values to compare.
#' @return logical
#' @noRd
is_growth_same_sign <- function(x, y) {
  (x[2] - x[1]) * (y[2] - y[1]) > 0
}



#' is this lineup year to interpolate?
#'
#' @param x numric: vector with survey years
#'
#' @return logical
#' @export
is_to_interpolate <- function(x) {

  length(x) %% 2 == 0

}



#' same_dir_growth
#'
#' Estimate growth factor for each distribution when growth in means is of the
#' same direction that growth of NACs to reference year.
#'
#'
#' @param svy_means numeric: vector of survey means
#' @param svy_nac   numeric: vector of National Accounts values
#' @param ref_nac   numeric: vector of national account value in reference year
#'
#' @return numeric vector
#' @export
same_dir_growth <- function(svy_means,
                            svy_nac,
                            ref_nac) {

  # early return
  l_sm <- length(svy_means)  # survey mean
  l_nv <- length(svy_nac)    # nac value

  # check reference year is unique
  ref_nac  <- unique(ref_nac)
  stopifnot(exprs = {
    length(ref_nac) == 1
    length(l_nv) == length(l_sm)
    l_sm %% 2 == 0
  })

  x <- matrix(svy_means, nrow = 2)
  y <- matrix(svy_nac, nrow = 2)

  gf <- vector(mode   = "numeric",
              length = ncol(x))


  a <- c(1,2)
  for (i in seq_along(x[1, ])) {
    gf[a] <- same_dir_growth_factor(survey_mean1 = x[1, i],
                                    survey_mean2 = x[2, i],
                                    svy_value1   = y[1, i],
                                    svy_value2   = y[2, i],
                                    ref_value    = ref_nac)
    a <-  a + 2
  }
  return(gf)

}


#' growth factor when interpolation is in the same direction
#'
#' @param survey_mean1 numeric: value of survey mean in year 1
#' @param survey_mean2 numeric: value of survey mean in year 2
#' @param svy_value1 numeric: value of national accounts in survey year 1
#' @param svy_value2 numeric: value of national accounts in survey year 2
#' @param ref_value numeric: value  of national accounts in reference year
#'
#' @return numeric vector of length 1
#' @export
same_dir_growth_factor <- function(survey_mean1,
                         survey_mean2,
                         svy_value1,
                         svy_value2,
                         ref_value) {

  # estimated reference mean for both years
  est_ref_mean <- (survey_mean2 - survey_mean1) *
    ((ref_value - svy_value1)/(svy_value2 - svy_value1)) + survey_mean1

  # growth factor
  est_ref_mean / c(survey_mean1, survey_mean2)

}

