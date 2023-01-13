library(RStata)
library(glue)
options(
  "RStata.StataPath" = "\"C:\\Program Files\\Stata17\\StataMP-64\"",
  RStata.StataVersion = 17
)

dta_files <- fs::dir_ls(path = "data/album/",
                        type = "file",
                        regexp = "dta$") |>
  fs::path_wd()

for (i in seq_along(dta_files)) {
  x <-dta_files[[i]]

  command <-
    glue('
        use {x}, clear
        label var reporting_level  "geographical area covered (urban, rural, national)"
        label var percentile       "percentile"
        label var avg_welfare      "welfare average of percentile"
        label var pop_share        "population share of percentile"
        label var welfare_share    "welfare share of percentile"
        label var quantile         "welfare upper threshold of percentile"
        label var country_code     "country code iso3"
        label var year             "year"
        label var welfare_type     "income or consumption"
       save, replace
       ')

  stata(command, stata.echo = FALSE)

}
