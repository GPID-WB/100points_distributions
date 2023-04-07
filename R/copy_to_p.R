if (Sys.info()["user"] == "wb384996" &&
    Sys.info()["nodename"] == "WBGMSDDG001") {

  dirs <- fs::dir_ls(path = "data/album/")

  fs::dir_copy(path = dirs,
               new_path = fs::path("p:/03.pip/estimates/percentiles",
                                   fs::path_file(dirs)),
               overwrite = FALSE
               )

}
