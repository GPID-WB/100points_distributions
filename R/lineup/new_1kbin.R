library(fastverse)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# run functions   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source("R/functions.R")
source("R/duplicate_households.R")
op <- options(joyn.reportvar = "report")


# remotes::install_github("PIP-Technical-Team/pipapi@DEV")

force <- TRUE

if (!"lkups" %in% ls() || isTRUE(force)) {
  data_dir <- Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL") |>
    fs::path()
  fs::dir_ls(data_dir, recurse = FALSE)
}


version <- "20260324_2017_01_02_PROD"
version <- "20260324_2021_01_02_PROD"

new_dir <-
  fs::path("p:/03.pip/estimates/1kbins_lineup", version) |>
  # fs::path("p:/03.pip/estimates/1kbins_lineup_temp", version) |>
  fs::dir_create(recurse = TRUE)

lkups <- pipapi::create_versioned_lkups(
  data_dir = data_dir,
  vintage_pattern = version
)


# lkup <-  lkups$versions_paths$`20230328_2011_02_02_PROD`
lkup <- lkups$versions_paths[[lkups$latest_release]]

nq <- 1000

refy <- copy(lkup$refy_lkup)

ni <- seq_len(nrow(refy))

# ni <- which(refy$country_code == "CHN" & refy$reporting_year == 1994)
# ni <- i <- ni[1]

llz <- lapply(cli::cli_progress_along(ni), \(i) {
  x <- refy$path[i]
  cd <- refy$country_code[i]
  yr <- refy$reporting_year[i]
  wt <- refy$welfare_type[i]

  dt <- fst::read_fst(x, as.data.table = TRUE)

  lt <- lorenz_table(dt, nq = nq)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Censoring --------

  lt <- lt[bin >= nq, quantile := NA_real_]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Creating id --------
  lt[,
    id := paste(cd, yr, wt, sep = "_")
  ]

  lt
})

cols <- c("id", "reporting_level", "bin")

dlt <- rbindlist(llz, fill = TRUE) |>
  setorderv(cols) |>
  setcolorder(cols)

dta_file <- fs::path(new_dir, "1kbins", ext = "dta")
haven::write_dta(dlt, dta_file)
fst::write_fst(dlt, fs::path_ext_set(dta_file, "fst"))


dlt <- fst::read_fst(fs::path_ext_set(dta_file, "fst"), as.data.table = TRUE)
