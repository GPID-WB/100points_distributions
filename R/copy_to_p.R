if (Sys.info()["user"] == "wb384996" &&
    Sys.info()["nodename"] == "WBGMSDDG001") {

  files <- fs::dir_ls(path = "data/album/",
             type = "file")
  names <- fs::path_file(files)

  to <- fs::path("p:/03.pip/estimates/percentiles", names)

  saved <-
    file.copy(from = files,
              to   = to,
              overwrite = TRUE)

}
