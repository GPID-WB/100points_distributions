
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run initial conditions   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("R/init.R")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# load all data and split   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## get file names --------


files_name <-
  fs::dir_ls(singles_dir,
             type = "file",
             regexp = glue("{ext}$")) |>
  fs::path_file() |>
  fs::path_ext_remove()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## find out bins groups and ppp years --------

vars <- c("country_code", "year", "welfare_type", "bins")
dt   <- data.table(file = files_name)

dt[, file_nosynth := gsub('synth_', '', file)]

dt[, (vars) := tstrsplit(file_nosynth, split = "_")]

bins <- dt[, unique(bins)]

# loop over bin groups
for (b in seq_along(bins)) {

  patter         <- glue("{bins[[b]]}")
  selected_files <- grep(patter, files_name, value = TRUE)

  if (length(selected_files) == 0) next # skip and go to next iteration

  file_paths     <- fs::path(singles_dir, selected_files, ext = ext)

  ### load data and append ---------
  ldist <- purrr::map(file_paths, qs::qread, .progress = TRUE)

  whole <- rbindlist(ldist, use.names = TRUE, fill = TRUE)
  whole[, year := as.numeric(year)]
  setnames(whole, "bin", "percentile")
  ovars <- c(
    "country_code",
    "year",
    "reporting_level",
    "welfare_type",
    "percentile"
  )
  setorderv(whole, ovars)
  setcolorder(whole, ovars)

  qs::qsave(whole, file = fs::path(album_dir, glue("world_{patter}"), ext = ext))

}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save in other formats   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Get qs world files
fs::dir_ls(path = album_dir,
           regexp = "world.*qs$") |>
  # load data
  map(qs::qread, .progress = TRUE) |>
  # get file names, remove qs, and save as dta
  {\(.) walk2(.x = names(.),
              .y = .,
              .f = ~{
                nm <- fs::path_ext_remove(.x)
                # save ad dta
                haven::write_dta(data = .y,
                                 path = fs::path(nm, ext = "dta"))
                # save as csv
                readr::write_csv(x = .y,
                                 file = fs::path(nm, ext = "csv"))
              })
    }()


if (require(pushoverr)) {
  pushoverr::pushover("Done creating world files")
}


