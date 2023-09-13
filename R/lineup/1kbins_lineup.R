library(fastverse)
pak::pak("PIP-Technical-Team/pipapi@DEV")
force <- TRUE

if (!"lkups" %in% ls() || isTRUE(force)) {
  data_dir <- Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL") |>
    fs::path()
  fs::dir_ls(data_dir, recurse = FALSE)
}

version  <- "20230626_2017_01_02_TEST"
lkups <- pipapi::create_versioned_lkups(data_dir = data_dir,
                                vintage_pattern = version)


new_dir <-
  fs::path("p:/03.pip/estimates/1kbins_lineup", version) |>
  fs::dir_create(recurse = TRUE)



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

# countries <- "PRY"
# pls <- c(1:5)
for (i in seq_along(countries)) {
  ct <- countries[i]
  nfile_name <- paste0(ct, "_1kbins_lineup")
  nfile_path <-
    new_dir |>
    fs::path(nfile_name, ext = "fst")

  if (fs::file_exists(nfile_path)) next

  lt <-
    lapply(pls,
           \(.) {
    pipapi::pip(country = ct,
                povline = .,
                lkup = lkup,
                fill_gaps = TRUE)
  }) |>
    rbindlist()

  fst::write_fst(lt, nfile_path)
}

