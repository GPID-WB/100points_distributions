# Playground to understand the functioning of the synth function

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

  wf <- wbpip:::sd_create_synth_vector(
    welfare = welfare,
    population = population,
    mean = mean,
    pop = pop,
    nobs = 100
  )

  return(wf)

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


