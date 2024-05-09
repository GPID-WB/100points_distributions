
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run initial conditions   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("R/init.R")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# load all data and split   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

singles_dir <-
album_dir <-

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## get file names --------

ext <- 'qs'
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

#dt[, (vars) := tstrsplit(file, split = "_")]

#bins <- dt[, unique(bins)]


## other version
library(stringr)

dt[, `:=` (
  country_code = str_extract(file, "^([A-Z]{3})"),
  year = as.integer(str_extract(file, "(\\d{4})")),
  welfare_type = str_extract(file, "(\\d{4}_)([a-z]+)(?=_((synth_)?\\d+bin))"),
  synth = ifelse(str_detect(file, "_synth_"), "synth", NA_character_),
  bins = str_extract(file, "(\\d+bin)$")
)]

dt[, welfare_type := str_remove(welfare_type, "\\d{4}_")]

bins <- dt[, unique(bins)]

# loop o ver bin groups
for (b in seq_along(bins)) {

  patter         <- glue("{bins[[b]]}")
  selected_files <- grep(patter, files_name, value = TRUE)

  if (length(selected_files) == 0) next # skip and go to next iteration

  file_paths     <- fs::path(singles_dir, selected_files, ext = ext)

  ### load data and append ---------
  ldist <- map(file_paths, qs::qread)

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
  map(qs::qread) |>
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


