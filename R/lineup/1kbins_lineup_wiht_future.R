

library(fastverse)
library(furrr)
library(progressr)

## Set for parallel processing
## Keep half cores for processes
## And the other half for sending parallel requests



# remotes::install_github("PIP-Technical-Team/pipapi@DEV")

force <- TRUE

if (!"lkups" %in% ls() || isTRUE(force)) {
  data_dir <- Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL") |>
    fs::path()
  fs::dir_ls(data_dir, recurse = FALSE)
}


version  <- "20240326_2017_01_02_PROD"
version  <- "20240429_2017_01_02_INT"
version  <- "20240627_2017_01_02_PROD"
version  <- "20250401_2017_01_02_PROD"
version  <- "20250401_2021_01_02_PROD"

new_dir <-
  fs::path("p:/03.pip/estimates/1kbins_lineup", version) |>
  # fs::path("p:/03.pip/estimates/1kbins_lineup_temp", version) |>
  fs::dir_create(recurse = TRUE)

lkups <- pipapi::create_versioned_lkups(data_dir = data_dir,
                                        vintage_pattern = version)




# lkup <-  lkups$versions_paths$`20230328_2011_02_02_PROD`
lkup <-  lkups$versions_paths[[lkups$latest_release]]


countries <- lkup$aux_files$countries$country_code
povlines <-
  seq(1:1e5)/200

lagpovline <- c(0, povlines[1:(length(povlines)-1)])

fct <- data.table(povlines = povlines)
fct[povlines > 50,
    fc := 1.0025
][is.na(fc),
  fc := 1]

rt <-
  fct |>
  ftransform(fpl = flag(povlines)) |>
  ftransform(avobe50 = fifelse(povlines > 50, 1, 0)) |>
  ftransform(multiplier = rowid(avobe50)) |>
  ftransform(fc = fc^(multiplier)) |>
  ftransform(new_pl = fc*fpl) |>
  fsubset(new_pl <= 900)

pls <- rt$new_pl

pls <- round(pls, 3) |>
  unique()

length(pls)

split_vector <- function(vec, x) {
  n <- ceiling(length(vec) / x)
  split(vec, rep(1:n, each = x, length.out = length(vec)))
}



pls2 <- split_vector(pls, 100)
length(pls2)  # Number of chunks



countries <-
  countries |>
  sort(decreasing = FALSE)


n_cores <- floor((availableCores() - 1) / 2) - 1
n_cores <- availableCores() - 2
n_cores <- 4
plan(multisession, workers = n_cores)



cols <- c(
  "country_code",
  "reporting_year",
  "reporting_level",
  "welfare_type",
  "poverty_line",
  "headcount",
  "poverty_gap",
  "poverty_severity"
)



# countries <- "NGA"
years <- "ALL"
years <- 1980:2025
years <- 2023:2025
years <- 2024:2025
# pls <- c(1:5)
# pls2 <- pls2[1:6]
# Run by LInes with Future ---------
options("pipapi.query_live_data" = FALSE)
tictoc::tic()
force <- FALSE
with_progress({
  p <- progressor(steps = length(pls2))

passed <- future_map(pls2,
                     \(pl){
                       p()
                       # cli::cli_alert_info("working on {ct}")
                       nfile_name <- paste0(pl, "_1kbins_lineup")
                       fst_file <-
                         new_dir |>
                         fs::path(nfile_name, ext = "fst")

                         tryCatch(
                           expr = {
                             if (any(!fs::file_exists(fst_file)) || force == TRUE) {
                             dt <- pipapi::pip(povline = pl,
                                               lkup = lkup,
                                               fill_gaps = TRUE,
                                               year = years) |>
                               fselect(cols)

                             lt <- split(dt, by = "poverty_line")

                             lapply(seq_along(lt), \(x) {
                               fst::write_fst(lt[[x]], fst_file[[x]])
                             })
                             }
                             TRUE
                           }, # end of expr section

                           error = function(e) {
                             FALSE
                           }, # end of error section

                           warning = function(w) {
                             FALSE
                           } # end of finally section

                         ) # End of trycatch
                     },
                     .options = furrr_options(seed = TRUE)
                     ) # end of map
})

if (require(pushoverr)) {
  pushoverr::pushover("Done with 1kbins")
}

plan(sequential)
toc <- tictoc::toc()
(toc$toc - toc$tic)/60

which(passed == FALSE)

### convert o Stata format ---------

tictoc::tic()

force = TRUE
fst_files <- new_dir |>
  fs::dir_ls(regexp = "fst$",
             recurse = FALSE,
             type = "file")

purrr::map(fst_files, \(x) {
  tryCatch(
    expr = {
      # Your code...
      y <- fst::read_fst(x,
                         columns  = cols,
                         as.data.table = TRUE)

      # dta_file <- x |>
      #   fs::path_ext_remove() |>
      #   fs::path(ext = "dta")
      #
      # if (force == TRUE || !fs::file_exists(dta_file)) {
      #   haven::write_dta(y, dta_file)
      # }
      y

    }, # end of expr section

    error = function(e) {
      NULL
    }, # end of error section

    warning = function(w) {
      NULL
    } # end of finally section
  )},
  .progress = TRUE) |>
  rbindlist() |>
  setorderv(cols = c("country_code",
                     "reporting_year",
                     "reporting_level",
                     "welfare_type",
                     "poverty_line")
  ) |>
  haven::write_dta(fs::path(new_dir, "1kbins", ext = "dta"))

toc <- tictoc::toc()
toc


if (require(pushoverr)) {
  pushoverr::pushover("Done copying 1kbins to dta")
}


################## DO NOT USE THE CODE BELOW ########################









