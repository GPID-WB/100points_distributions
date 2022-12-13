
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run initial conditions   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("R/init.R")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# load all data and split   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

singles_dir <- "data/singles"
album_dir <- "data/album"
ext <- "qs"


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

vars <- c("country_code", "year", "welfare_type", "bins", "ppp_year")
dt   <- data.table(file = files_name)

dt[, (vars) := tstrsplit(file, split = "_")]

bins <- dt[, unique(bins)]
ppps <- dt[, unique(ppp_year)]

#loop over PPP years

for (p in seq_along(ppps)) {
  # loop o ver bin groups
  for (b in seq_along(bins)) {

    patter         <- glue("{bins[[b]]}_{ppps[[p]]}")
    selected_files <- grep(patter, files_name, value = TRUE)
    file_paths     <- fs::path(singles_dir, selected_files, ext = ext)

    ### load data and append ---------
    ldist <- map(file_paths, qs::qread)

    whole <- rbindlist(ldist, use.names = TRUE)

    qs::qsave(whole, file = fs::path(album_dir, glue("world_{patter}"), ext = ext))

  }
}

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
                haven::write_dta(data = .y,
                                 path = fs::path(nm, ext = "dta")
                )
              })}()
