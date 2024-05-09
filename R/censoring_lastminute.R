# Emergency censoring and adding back rural-urban

# 1. Rename files in directory ----
singles_temp_dir <- 'P:/03.pip/estimates/percentiles/singles/20240326_2011_02_02_PROD_synth'


# List files in the directory ending with '100bin.qs'
files_name <- fs::dir_ls(singles_temp_dir, type = "file", regexp = "100bin\\.qs$")

# Loop through the files and rename each by adding 'synth_' prefix before '100bin.qs'
for (file_path in files_name) {
  # Get the base name without extension
  base_name <- fs::path_ext_remove(fs::path_file(file_path))

  # Construct the new file name by inserting 'synth_' before '100bin'
  new_base_name <- sub("100bin", "synth_100bin", base_name)

  # Add back the extension
  new_file_name <- paste0(new_base_name, ".qs")

  # Build the full path for the new file name
  new_file_path <- fs::path(singles_temp_dir, new_file_name)

  # Rename the file
  fs::file_move(file_path, new_file_path)
}


# 2. Load .csv and censor observations----
world_path <- 'C:/WBG/repos/100points_distributions/data/album/20240326_2011_02_02_PROD/world_100bin.csv'
world_csv <- read.csv(world_path)
world_csv |>
  mutate(quantile = ifelse(percentile == 100, NA_real_, quantile))-> world_csv
write.csv(world_csv, "C:/WBG/repos/100points_distributions/data/album/20240326_2011_02_02_PROD/world_100bin_censored.csv")
haven::write_dta(world_csv, "C:/WBG/repos/100points_distributions/data/album/20240326_2011_02_02_PROD/world_100bin.dta")

