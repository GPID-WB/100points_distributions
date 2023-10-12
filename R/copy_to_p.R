if (Sys.info()["user"] == "wb384996" &&
    Sys.info()["nodename"] == "WBGMSDDG001") {


  new_dir <-
    fs::path("p:/03.pip/estimates/percentiles", version) |>
    fs::dir_create()

  fs::dir_copy(path = album_dir,
               new_path = new_dir,
               overwrite = TRUE)

}

