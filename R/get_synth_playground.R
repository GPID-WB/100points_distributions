# Playground to understand the functioning of the synth function

# GET SYNTH
# Step 1:  Check the synth function ----

id <- "CHN_2020_consumption"
level <- 'rural'
vctr <- vctrs[[id]]
mean <- mean_ppp[[id]]
pop_table <- pop_list[[id]]
welfare_level <- vctr$welfare[[level]]
population_level <- vctr$population[[level]]
mean_level <- mean[[level]]
pop_level <- pop_table[[level]]



wf_rural <- wbpip:::sd_create_synth_vector(
  welfare = welfare_level,
  population = population_level,
  mean = mean_level,
  pop = pop_level
)

# Step 2: Inner map to levels ----

get_synth_level <- function(level,
                            vctr,
                            mean,
                            pop) {

  welfare <- vctr$welfare[[level]]
  population <- vctr$population[[level]]
  mean <- mean[[level]]
  pop <- pop[[level]]

  synth_vector_level <- wbpip:::sd_create_synth_vector(
    welfare = welfare,
    population = population,
    mean = mean,
    pop = pop,
    nobs = 100
  )


  return(synth_vector_level)

}



levels <- c('rural', 'urban')

wf <- purrr::map(
  .x = levels,
  .f = ~ get_synth_level(
    level = .x,
    vctr = vctr,
    mean = mean,
    pop = pop_table)
  ) |>
  rbindlist() |>
  setorder(welfare)


# Step 3: Outer map to ids ----

outer_wf <- map(
  .x = names(vctrs),
  .f = ~{
    # Extract details for the current id
    id <- .x
    mean_id <- mean_ppp[[id]]
    vctr_id <- vctrs[[id]]
    pop_table_id <- pop_list[[id]]

    # Get the names of the levels
    levels <- names(mean_id)

    # Map over levels and apply the get_synth_level function
    wf_id <- purrr::map(
      .x = levels,
      .f = ~get_synth_level(
        level = .x,
        vctr = vctr_id,
        mean = mean_id,
        pop = pop_table_id
      )
    ) |>
      rbindlist() |>
      setorder(welfare)
  }
)

# Assign the names from vctrs to the outer_wf result
names(outer_wf) <- names(vctrs)



# 4. Rbindlist inside get_synth -----
get_synth_level_rbind <- function(vctr,
                                  mean,
                                  pop) {

  # Extract levels
  levels <- names(mean)

  # Map to each level
  synth_level <- purrr::map(
    levels,
    ~{
      level <- .x
      # Extract data for the current level
      welfare <- vctr$welfare[[level]]
      population <- vctr$population[[level]]
      mean_level <- mean[[level]]
      pop_level <- pop[[level]]

      # Create synthetic vector for this level
      synth_vector_level <- wbpip:::sd_create_synth_vector(
        welfare = welfare,
        population = population,
        mean = mean_level,
        pop = pop_level
      )
      return(synth_vector_level)
    }
  )

  # Combine all the results into a single data.table
  combined_results <- purrr::reduce(synth_level, rbind)|>
    setorder(welfare)


}


outer_wf <- map(
  .x = names(vctrs),
  .f = ~{
    id <- .x
    mean_id <- mean_ppp[[id]]
    vctr_id <- vctrs[[id]]
    pop_table_id <- pop_list[[id]]


    wf_id <- get_synth_level_rbind(
      vctr = vctr_id,
      mean = mean_id,
      pop = pop_table_id
    )

    return(wf_id)
  }
)


names(outer_wf) <- names(vctrs)


# 5. Calculate actual percentiles  ----

## 5.1 Procedure ----
dt <- outer_wf$CHN_2020_consumption

### bins
dt[,
   # get bins and total pop and welfare

   bin := wbpip:::md_compute_bins(welfare,
                                  weight,
                                  nbins = nq,
                                  output = "simple")][,
  `:=`(
    tot_pop = sum(weight),
    tot_wlf = sum(welfare*weight)
  )
]


### measures
dt[,
   # get avg wlf, pop and wlf shared
   c("avg_welfare", "pop_share", "welfare_share", "quantile") := {
     avg_welfare   <-  weighted.mean(welfare, weight)
     pop_share     <- sum(weight)/tot_pop
     welfare_share <- sum(welfare*weight)/tot_wlf
     quantile      <- max(welfare)
     list(avg_welfare, pop_share, welfare_share, quantile)
   },
   by = .(bin)]

## mean
dt <-
  dt[,
     # mean by imputation and bin
     lapply(.SD, mean),
     by = .(bin),
     .SDcols =  c("avg_welfare", "pop_share", "welfare_share", "quantile")
  ][,
    # mean by bin
    lapply(.SD, mean),
    by = .(bin),
    .SDcols =  c("avg_welfare", "pop_share", "welfare_share", "quantile")
  ]

## 5.2 Mapped ----
outer_wf <- map(
  .x = names(vctrs),
  .f = ~{
    id <- .x
    mean_id <- mean_ppp[[id]]
    vctr_id <- vctrs[[id]]
    pop_table_id <- pop_list[[id]]

    # Create rbinded synthetic level data for each id
    wf_id <- get_synth_level_rbind(
      vctr = vctr_id,
      mean = mean_id,
      pop = pop_table_id
    )

    nbins <- 100

    # Compute bins, tot_pop and tot_wlf
    wf_id[, bin := wbpip:::md_compute_bins(welfare,
                                           weight,
                                           nbins = nbins,
                                           output = "simple")][
      , `:=`(tot_pop = sum(weight, na.rm = TRUE),
             tot_wlf = sum(welfare * weight, na.rm = TRUE))
    ]

    # Compute measures
    wf_id[, c("avg_welfare", "pop_share", "welfare_share", "quantile") := {
      avg_welfare   <- weighted.mean(welfare, weight, na.rm = TRUE)
      pop_share     <- sum(weight, na.rm = TRUE) / tot_pop
      welfare_share <- sum(welfare * weight, na.rm = TRUE) / tot_wlf
      quantile      <- max(welfare, na.rm = TRUE)
      .(avg_welfare, pop_share, welfare_share, quantile)
    }, by = .(bin)]

    # Mean by bin
    wf_id <- wf_id[,
                   lapply(.SD, mean, na.rm = TRUE),
                   by = .(bin),
                   .SDcols = c("avg_welfare", "pop_share", "welfare_share", "quantile")
                   ]

    return(wf_id)
  }
)


## 5.3 Mapped v2 ----
# Define a single function to process data
create_synth_bins <- function(vctr,
                              mean,
                              pop,
                              nbins) {

  # Get synthetic distribution
  dt <- get_synth_level_rbind(
    vctr = vctr,
    mean = mean,
    pop = pop
  )

  # Compute bins, tot_pop and tot_wfl
  dt[, bin := wbpip:::md_compute_bins(welfare,
                                      weight,
                                      nbins = nbins,
                                      output = "simple")]
  dt[, `:=`(tot_pop = sum(weight, na.rm = TRUE),
            tot_wlf = sum(welfare * weight, na.rm = TRUE))]

  # Compute measures
  dt[, c("avg_welfare", "pop_share", "welfare_share", "quantile") := {
    avg_welfare   <- weighted.mean(welfare, weight, na.rm = TRUE)
    pop_share     <- sum(weight, na.rm = TRUE) / tot_pop
    welfare_share <- sum(welfare * weight, na.rm = TRUE) / tot_wlf
    quantile      <- max(welfare, na.rm = TRUE)
    .(avg_welfare, pop_share, welfare_share, quantile)
  }, by = .(bin)]

  # Mean by bin
  dt <- dt[,
           lapply(.SD, mean, na.rm = TRUE),
           by = .(bin),
           .SDcols = c("avg_welfare", "pop_share", "welfare_share", "quantile")]

  return(dt)
}

# Map
gd_synth_bins <- map(
  .x = names(vctrs),
  .f = ~{
    id <- .x
    mean_id <- mean_ppp[[id]]
    vctr_id <- vctrs[[id]]
    pop_table_id <- pop_list[[id]]


    wf_id <- create_synth_bins(
      vctr = vctr_id,
      mean = mean_id,
      pop = pop_table_id,
      nbins = 100
    )

    return(wf_id)
  }
)







fpf[, ]


# 6. Micro data procedure ----
## Load single dt
dt   <- pipload::pip_load_cache('IND',
                                2019,
                                verbose = FALSE,
                                version = version,
                                welfare_type = 'CON')

setorder(dt, imputation_id, welfare_type,
         welfare_ppp, weight)

## Exit if dt has national
unique_levels <- unique(dt$reporting_level)
if ('national' %in% unique_levels) {
  return(NULL)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## bins ant totals --------

dt[,
   # get bins and total pop and welfare
   bin := wbpip:::md_compute_bins(welfare_ppp,
                                  weight,
                                  nbins = nq,
                                  output = "simple"),
   by = c("imputation_id", "welfare_type") # remove reporting level
][,
  `:=`(
    tot_pop = sum(weight),
    tot_wlf = sum(welfare_ppp*weight)
  ),
  by = c("imputation_id", "welfare_type") # remove reporting level
]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## main measures --------

dt[,
   # get avg wlf, pop and wlf shared
   c("avg_welfare", "pop_share", "welfare_share", "quantile") := {
     avg_welfare   <-  weighted.mean(welfare_ppp, weight)
     pop_share     <- sum(weight)/tot_pop
     welfare_share <- sum(welfare_ppp*weight)/tot_wlf
     quantile      <- max(welfare_ppp)
     list(avg_welfare, pop_share, welfare_share, quantile)
   },
   by = .(imputation_id, welfare_type,  bin)]

dt <-
  dt[,
     # mean by imputation and bin
     lapply(.SD, mean),
     by = .(imputation_id, welfare_type, bin),
     .SDcols =  c("avg_welfare", "pop_share", "welfare_share", "quantile")
  ][,
    # mean by bin
    lapply(.SD, mean),
    by = .(welfare_type, bin),
    .SDcols =  c("avg_welfare", "pop_share", "welfare_share", "quantile")
  ]


