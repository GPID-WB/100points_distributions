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

  if (region_code == "SSA" || anyNA(c(survey_pce, ref_pce)))  {
    if (length(survey_gdp) == 1) {
      proxy <- list(
        svy_value1 = survey_gdp,
        svy_value2 = NULL,
        ref_value  = ref_gdp
      )
    } else {
      proxy <- list(
        svy_value1 = survey_gdp[1],
        svy_value2 = survey_gdp[2],
        ref_value  = ref_gdp
      )
    }
  } else {
    # For other countries, use PCE if available
      if (length(survey_pce) == 1) {
        proxy <- list(
          svy_value1 = survey_pce,
          svy_value2 = NULL,
          ref_value  = ref_pce
        )
      } else {
        proxy <- list(
          svy_value1  = survey_pce[1],
          svy_value2  = survey_pce[2],
          ref_value   = ref_pce
        )
      }
  }
  return(proxy)
}
