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

lorenz_table <- \(x, nq = 100) {
  # number of data labels
  no_dl <- funique(x$reporting_level) |>
    length()

  df <- x |>
  ## totals -----------
  ftransform(tot_pop = fsum(weight),
             tot_wlf = fsum(welfare*weight))

  ## Bins at reporting level -------
  # sort according to bins calculation method
  setorder(df, reporting_level, welfare)
  dt <-  df |>
    fgroup_by(reporting_level) |>
    fmutate(bin = new_bins(welfare = welfare,
                           weight = weight,
                           nbins = nq)) |>
    fungroup()


  # if there is  more than one reporting level
  if (no_dl > 1) {
    ## Bins at national level ----
    dt <-  df |>
      ftransform(bin = new_bins(welfare = welfare,
                                weight = weight,
                                nbins = nq),
                 reporting_level = "national")  |>
      rowbind(dt)
  }



  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Main measures --------

  dt <- dt |>
    ## shares at the observation level ---------
  ftransform(pop_share = weight/tot_pop,
             welfare_share = (welfare*weight)/tot_wlf) |>
    ## aggregate
    fgroup_by(reporting_level, bin) |>
    fsummarise(avg_welfare    = fmean(welfare, w = weight),
               pop_share      = fsum(pop_share),
               welfare_share  = fsum(welfare_share),
               quantile       = fmax(welfare),
               pop            = fsum(weight)) |>
    fungroup()

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




new_bins <- \(welfare, weight, nbins) {
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
