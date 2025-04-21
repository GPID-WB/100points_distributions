# library(ggplot2)
library(data.table)
library(collapse)

# Step 1: Identify Outliers
find_outliers <- \(DT, weight = "weight", threshold = 2.5) {
  mean_w <- fmean(DT[[weight]])
  sd_w   <- fsd(DT[[weight]])

  DT[, is_outlier := .SD[[1]] > (mean_w + threshold * sd_w), .SDcols = weight]
  DT
}


optimize_ratio <- \(y, m) {
  # Candidate optimum (smallest x possible given that
  # 1. y/x < x, whichi x > y^(1/2)
  # 2. x > m, hwere m is the min or mean of the distribution

  opt_x <- pmax(m, sqrt(y))

  # Check the extra condition: we need x_opt <= y/2 to ensure y/x >= 2.
  to_one <- (opt_x > y/2)
  opt_x[to_one] <- 1

  round(y / opt_x)

}


# Step 2: Calculate Replications and Partitioning for Outliers
duplicate_obs <- \(DT, weight = "weight") {
  min_w <- fmin(DT[[weight]])

  # Get houdehold ID
  DT[, hhindex := .I]
  # Get replication count
  DT[, rep_count := optimize_ratio(.SD[[1]],  min_w),
     .SDcols = weight
  ][is_outlier == FALSE,
    rep_count := 1]

  Y <- DT[rep(1:.N,rep_count)]
  Y
}


split_weights <- \(x, rep) {
  base <- round(x / rep)
  rem <- x[1] - fsum(base)

  add_to_base <- floor(abs(rem)/base[1])

  base[length(base)] <- base[1] + rem
  base
}

add_new_weights <- \(DT, weight = "weight") {
  X <- DT[is_outlier == TRUE]
  Y <- DT[is_outlier == FALSE]
  # X[, x := split_weights(.SD[[1]], rep_count),
  #    by = hhindex,
  #    .SDcols = weight]
  X[, x := .SD[[1]]/ rep_count,
    .SDcols = weight]
  setnames(X, c(weight, "x"), c("x", weight)) # reverse names
  X[, x := NULL]

  rowbind(Y,X, fill = TRUE)
}

clean_new_weights <- \(DT, ori_names) {
  DT <- DT[, ..ori_names]
}

lorenz_table <- \(df, nq = 100) {
  # number of data labels
  no_dl <- funique(df$reporting_level) |>
    length()


  if (no_dl > 1) {
## Bins at national level ----
    df2 <-  copy(df)
    df2[, reporting_level := "national"]
    df <- rowbind(df, df2)
  }

  ## Bins at reporting level -------
  # sort according to bins calculation method
  setorder(df, reporting_level, welfare)
  df[, id := .I]

  df2 <- df[reporting_level == "urban"]


  bin_df <- df2[, new_bins(welfare = welfare,
                          weight = weight,
                          id = id,
                          nbins = nq),
               by = reporting_level]




  dt <-  df |>
    setorder(reporting_level, welfare) |>
    ftransform(wt_welfare = welfare*weight) |>
    fgroup_by(reporting_level) |>
    fmutate(bin = new_bins(welfare = welfare,
                           weight = weight,
                           nbins = nq),
  ## totals by reporting level
            tot_pop = fsum(weight),
            tot_wlf = fsum(wt_welfare)) |>
    fungroup() |>
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Main measures --------
  ## shares at the observation level ---------
  ftransform(pop_share = weight/tot_pop,
             welfare_share = (wt_welfare)/tot_wlf) |>
    ## aggregate
    fgroup_by(reporting_level, bin) |>
    fsummarise(avg_welfare    = fmean(welfare, w = weight),
               pop_share      = fsum(pop_share),
               welfare_share  = fsum(welfare_share),
               quantile       = fmax(welfare),
               pop            = fsum(weight)) |>
    fungroup() |>
    setorder(reporting_level, bin)

  dt

}

#' Assign households to weighted bins (percentiles)
#'
#' This function assigns each household to a percentile bin using a weighted distribution,
#' ensuring that each bin has approximately the same total weight. Households with large
#' weights may be duplicated and split across bins to preserve monotonicity of welfare shares.
#'
#' @param welfare Numeric vector of household welfare (e.g., income).
#' @param weight  Numeric vector of sampling weights (same length as welfare).
#' @param nbins   Number of bins to divide the population into (default = 100).
#' @param id      Optional ID vector for each observation. If NULL, it will be generated.
#'
#' @return A `data.table` with columns: `id`, `welfare`, `weight`, and `bin`.
#'   The output may have more rows than the input due to household splitting.
#'
#' @export
new_bins <- function(welfare, weight, id, nbins = 100, tolerance = 1e-6) {
  stopifnot(length(welfare) == length(weight),
            length(weight) == length(id))

  # ~~~ Handle NAs ~~~
  keep <- !is.na(welfare) & !is.na(weight)
  welfare <- welfare[keep]
  weight  <- weight[keep]
  id      <- id[keep]

  # ~~~ Sort by welfare ~~~
  o <- order(welfare)
  welfare <- welfare[o]
  weight  <- weight[o]
  id      <- id[o]

  # ~~~ Define bin size ~~~
  total_weight <- sum(weight)
  bin_size     <- total_weight / nbins

  # ~~~ Initialize outputs with some padding ~~~
  n_est <- length(welfare) * 2
  out_id      <- vector("integer", n_est)
  out_welfare <- vector("numeric", n_est)
  out_weight  <- vector("numeric", n_est)
  out_bin     <- vector("integer", n_est)

  j <- 1  # index for outputs
  current_bin     <- 1
  accumulated_bin <- 0

  for (i in seq_along(welfare)) {
    w   <- weight[i]
    val <- welfare[i]
    uid <- id[i]

    while (w > 0) {
      space <- bin_size - accumulated_bin

      # If the last bin, allow a small tolerance
      if (current_bin == nbins && w > space && abs(space - bin_size) < tolerance) {
        portion <- w  # Dump remaining weight in last bin
      } else {
        portion <- min(w, space)
      }

      out_id[j]      <- uid
      out_welfare[j] <- val
      out_weight[j]  <- portion
      out_bin[j]     <- current_bin

      w <- w - portion
      accumulated_bin <- accumulated_bin + portion
      j <- j + 1

      if (accumulated_bin >= bin_size - tolerance && current_bin < nbins) {
        current_bin     <- current_bin + 1
        accumulated_bin <- 0
      }
    }
  }

  # ~~~ Build result ~~~
  result <- data.table::data.table(
    id      = out_id[1:(j - 1)],
    welfare = out_welfare[1:(j - 1)],
    weight  = out_weight[1:(j - 1)],
    bin     = out_bin[1:(j - 1)]
  )

  return(result)
}








# Wrapper Function
duplicate_households <- function(DT,
                                 weight = "weight",
                                 threshold = 2.5,
                                 i = 0,
                                 li = 5,
                                 super_limit = 20) {
  R <- copy(DT)  # work on a copy to avoid modifying original DT

  # First iteration to make avoid goind through the whole
  # transformation of weights
  if (i == 0) {
    lt <- lorenz_table(R) # this is very inefficient, but that's what I have for now
    welfare_share_bad <-
      lt[, diff(lt$welfare_share),
         by = reporting_level
      ][, any(V1 < 0)]

    setattr(R, "welfare_share_OK", !welfare_share_bad)
    setattr(R, "threshold", threshold)
    setattr(R, "iterations", i)
    setattr(R, "lorenz", lt)
    if (!welfare_share_bad) {
      return(R)
    }
  }

  if (i >= super_limit)
    return(R)


  i = i + 1
  ori_names <- R |>
    names() |>
    copy()
  R <- find_outliers(R, weight, threshold)
  R <- duplicate_obs(R, weight)
  R <- add_new_weights(R, weight)
  R <- clean_new_weights(R, ori_names)
  lt <- lorenz_table(R) # this is very inefficient, but that's what I have for now

  welfare_share_bad <-
    lt[, diff(lt$welfare_share),
       by = reporting_level
    ][, any(V1 < 0)]

  setattr(R, "lorenz", lt)
  setattr(R, "welfare_share_OK", !welfare_share_bad)
  setattr(R, "threshold", threshold)
  setattr(R, "iterations", i)

  # if after li iteration still does nor, then lower the threshold and
  # increase the number of iteration
  if (welfare_share_bad && i >= li && threshold > 0) {
    threshold <- max(threshold - .5, 0)
    li <- li*2
  }

  if (welfare_share_bad) {
    R <- duplicate_households(R, weight = weight,
                              threshold = threshold,
                              i = i,
                              li = li,
                              super_limit = super_limit)
  }

  R

}




new_bins_old <- \(welfare, weight, nbins) {
  # deal with NAs -----
  if (anyNA(welfare)) {
    ina      <- !is.na(welfare)
    weight   <- weight[ina]
    welfare  <- as.numeric(welfare)[ina]
  }

  if (anyNA(weight)) {
    ina      <- !is.na(weight)
    weight   <- weight[ina]
    welfare  <- as.numeric(welfare)[ina]
  }

  # Sort data ------

  if (is.unsorted(welfare)) {
    o       <- order(welfare) # this is faster than collapse::radixorder
    welfare <- welfare[o]
    weight  <- weight[o]
  }

  p <- fcumsum(weight)/fsum(weight)
  bins <- ceiling(p * nbins) # Bins
  bins[bins > nbins] <- nbins
  bins
  # bins <-  cut(p, c(0, probs), labels = FALSE)
}




# civ <- pipload::pip_load_cache("CIV", 2002)
# civ2 <- replicate_households(civ)
#
# attributes(civ2)
# nrow(civ2)
# nrow(civ)
#
# wbpip::md_compute_gini(civ$welfare, civ$weight)
# wbpip::md_compute_gini(civ2$welfare, civ2$weight)
#
#
# ury <- pipload::pip_load_cache("PRY", 2018)
# ury2 <- replicate_households(ury)
#
#
#
# civ2  |>
#   ggplot(aes(x = weight)) +
#     geom_histogram(bins = 100) +
#     geom_vline(aes(xintercept=fsd(weight)*2.5+fmean(weight)),
#                color="blue", linetype="dashed", linewidth=1)
#
#
#
#
#
# civp1 <- lorenz_table(civ)
# civp2 <- lorenz_table(civ2)
#
# perr <- which(diff(civp1$welfare_share) < 0)
# perr2 <- which(diff(civp2$welfare_share) < 0)
#
# civp1[perr]
# civp1[perr2]
# civp2[c(perr2, perr2+1) |> sort()]
#
# dim(civ2)
