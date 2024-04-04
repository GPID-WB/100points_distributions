

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

new_dir <-
  fs::path("p:/03.pip/estimates/1kbins_lineup", version) |>
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

countries <-
  countries |>
  sort(decreasing = TRUE)

n_cores <- floor((availableCores() - 1) / 2)
plan(multisession)



# countries <- c("HND", "PRY")
# pls <- c(1:5)

# Run by LInes with Future ---------

with_progress({
  p <- progressor(steps = length(countries))

  future_walk(pls,
              \(pl){
                p()
                # cli::cli_alert_info("working on {ct}")
                nfile_name <- paste0(pl, "_1kbins_lineup")
                fst_file <-
                  new_dir |>
                  fs::path(nfile_name, ext = "fst")

                dta_file <-
                  new_dir |>
                  fs::path(nfile_name, ext = "dta")

                if (!fs::file_exists(fst_file)) {
                  lt <- pipapi::pip(povline = pl,
                                   lkup = lkup,
                                   fill_gaps = TRUE)
                  fst::write_fst(lt, fst_file)
                  # haven::write_dta(lt, dta_file)
                }
              },
              .options = furrr_options(seed = TRUE)
  )
})


if (require(pushoverr)) {
  pushoverr::pushover("Done with 1kbins")
}

plan(sequential)


### convert o Stata format ---------

tictoc::tic()
new_dir |>
  fs::dir_ls(regexp = "fst$",
             recurse = FALSE,
             type = "file") |>
  purrr::map(\(x) {
    y <- fst::read_fst(x, as.data.table = TRUE)
    y[, c(
      "country_code",
      "reporting_year",
      "reporting_level",
      "welfare_type",
      "poverty_line",
      "headcount",
      "poverty_gap",
      "poverty_severity"
    )]

    haven::write_dta(y, fs::path(fs::path_ext_remove(x), ext = "dta"))
    y
    }) |>
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





# Run by countries with Future ---------

with_progress({
  p <- progressor(steps = length(countries))

  future_walk(countries,
             \(ct){
               p()
               # cli::cli_alert_info("working on {ct}")
               nfile_name <- paste0(ct, "_1kbins_lineup")
               fst_file <-
                 new_dir |>
                 fs::path(nfile_name, ext = "fst")

               dta_file <-
                 new_dir |>
                 fs::path(nfile_name, ext = "dta")

               if (!fs::file_exists(fst_file)) {
                 lt <-
                   purrr::map(pls,
                          \(x) {
                            pipapi::pip(country   = ct,
                                        povline   = x,
                                        lkup      = lkup,
                                        fill_gaps = TRUE)
                          }) |>
                   rbindlist(use.names = TRUE, fill = TRUE)

                 fst::write_fst(lt, fst_file)
                 haven::write_dta(lt, dta_file)
               }
             },
             .options = furrr_options(seed = TRUE)
             )
})


if (require(pushoverr)) {
  pushoverr::pushover("Done with 1kbins")
}

plan(sequential)




x <- "PRY_1kbins_lineup"

dt <-
  fs::path(new_dir, x, ext = "fst") |>
  fst::read_fst(as.data.table = TRUE)

qsu(dt, poverty_line ~ reporting_year)



# only IND ---------

tictoc::tic()
with_progress({
  p <- progressor(steps = length(pls))
  ct <- "IND"
  lt <-
    future_map(pls,
               \(.) {
                 pipapi::pip(country = ct,
                             povline = .,
                             lkup = lkup,
                             fill_gaps = TRUE)
               },
               .options = )
})

tt <- tictoc::toc()

lt <- rbindlist(lt)

nfile_name <- paste0(ct, "_1kbins_lineup")
nfile_path <-
  new_dir |>
  fs::path(nfile_name, ext = "fst")

fst::write_fst(lt, nfile_path)
plan(sequential)


# Run Sequentially -------------

for (i in seq_along(countries)) {
  ct <- countries[i]
  cli::cli_alert_info("working on {ct}")
  nfile_name <- paste0(ct, "_1kbins_lineup")
  nfile_path <-
    new_dir |>
    fs::path(nfile_name, ext = "fst")

  if (fs::file_exists(nfile_path)) next

  lt <-
    lapply(cli::cli_progress_along(pls),
           \(.) {
             pipapi::pip(country = ct,
                         povline = .,
                         lkup = lkup,
                         fill_gaps = TRUE)
           }) |>
    rbindlist()

  fst::write_fst(lt, nfile_path)
}

if (require(pushoverr)) {
  pushoverr::pushover("Done with 1kbins")
}



# save to dta -------


save <- TRUE

if (save) {

  file_names <- new_dir |>
    fs::dir_ls(type = "file",
               regexp = "fst$") |>
    fs::path_file() |>
    fs::path_ext_remove() |>
    sort()

  purrr::map(cli::cli_progress_along(file_names),
             \(.) {
               x <- file_names[.]
               dta_name <- fs::path(new_dir, x, ext = "dta")
               if (!fs::file_exists(dta_name)) {
                 fs::path(new_dir, x, ext = "fst") |>
                   fst::read_fst()
               } else {
                 NULL
               }
             }) |>
    purrr::walk2(.y = file_names,
                 .f = \(x, y) {
                   dta_name <- fs::path(new_dir, y, ext = "dta")
                   if (!fs::file_exists(dta_name))
                     haven::write_dta(x, fs::path(new_dir, y, ext = "dta"))
                 })
}

