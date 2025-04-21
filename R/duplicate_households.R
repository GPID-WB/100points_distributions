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


new_bins <- function(welfare, weight, nbins = 100) {
  # Step 1: Sort data by welfare so bins are filled from poorest to richest
  # Percentile-type bins must be assigned in welfare order for meaningful
  # results.
  o <- order(welfare)
  welfare <- welfare[o]
  weight  <- weight[o]

  # Step 2: Compute total weight and target weight per bin WHY: We want each bin
  # to represent an equal *share of the population*, not a count of rows.
  total_weight <- sum(weight)
  bin_size     <- total_weight / nbins

  # Step 3: Set up storage and counters
  # bin_assignments: will store the bin number for each (possibly split) row
  # current_bin: index of the bin we're filling
  # accumulated: running total of the weight currently assigned to current_bin
  bin_assignments <- integer(0)
  current_bin     <- 1
  accumulated     <- 0

  # Step 4: Loop over each household (row in original data)
  for (i in seq_along(welfare)) {
    wt <- weight[i]  # Total weight for this household

    # A household might be "too heavy" to fit in the current bin,
    # so we may need to split it across multiple bins.
    while (wt > 0 && current_bin <= nbins) {

      # Step 4a: How much room is left in the current bin?
      # We don’t want to overfill it — we aim for evenly distributed bins.
      room <- bin_size - accumulated

      # Step 4b: How much of this household can we assign here?
      # Portion is the smaller of the household's remaining weight or bin space left.
      portion <- min(wt, room)

      # Step 4c: Assign this portion to the current bin
      # WHY: Even if a household is split across bins, each *portion* gets a bin label.
      bin_assignments <- c(bin_assignments, current_bin)

      # Step 4d: Update the bin's accumulated weight
      accumulated <- accumulated + portion

      # Step 4e: Decrease the unassigned portion of this household's weight
      wt <- wt - portion

      # Step 4f: If the bin is full, move to next one
      # THis Enforces our equal-weight constraint across bins.
      if (accumulated >= bin_size && current_bin < nbins) {
        current_bin <- current_bin + 1
        accumulated <- 0  # reset for the next bin
      }
    }
  }

  # Step 5: Return the vector of bin assignments
  # Each entry in this vector corresponds to a portion of the original data.
  # If splitting occurred, you’ll have more rows than the original input.
  return(bin_assignments)
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
