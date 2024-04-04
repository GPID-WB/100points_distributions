# Load libraries -----------
library(fastverse)
set_collapse(
  nthreads = 4,
  sort = FALSE,
  mask = c("%in%"),
  remove = "old"
)
# Final conflicts check (optional)
fastverse_conflicts()

# pak::pak("PIP-technical-team/pipapi@DEV")
# pak::pak("PIP-technical-team/pipapi@DEV")

# setup ------------

## directories -----------
force <- FALSE

if (!"lkups" %in% ls() || isTRUE(force)) {
  data_dir <- Sys.getenv("PIPAPI_DATA_ROOT_FOLDER_LOCAL") |>
    fs::path()
  fs::dir_ls(data_dir, recurse = FALSE)

  version  <- "20230919_2017_01_02_PROD"


  ## Lkup files ===========
  lkups <- pipapi::create_versioned_lkups(data_dir        = data_dir,
                                          vintage_pattern = version)
}

# ppp years
ppp_year <- py <- version |>
  gsub("(.*)_([0-9]{4})(_.*)", "\\2", x = _) |>
  as.numeric()
# lkup <-  lkups$versions_paths$`20230328_2011_02_02_PROD`
lkup <-  lkups$versions_paths[[lkups$latest_release]]

