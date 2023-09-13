if (Sys.info()["user"] == "wb384996" &&
    Sys.info()["nodename"] == "WBGMSDDG001") {

  dirs <- fs::dir_ls(path = "data/album/")

  new_dirs <-
    fs::path("p:/03.pip/estimates/percentiles",
           fs::path_file(dirs)) |>
    fs::dir_create()

  walk2(dirs, new_dirs,
        .f = \(x, y) {
          fs::dir_copy(path = x,
                       new_path = y,
                       overwrite = FALSE)
        })

}
