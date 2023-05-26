library(RStata)
library(glue)
options(
  "RStata.StataPath" = "\"C:\\Program Files\\Stata17\\StataMP-64\"",
  RStata.StataVersion = 17
)

relative_path <- fs::dir_ls(path = "data/album/",
                        type = "file",
                        regexp = "dta$",
                        recurse = TRUE)
versions <- gsub("(.+/)([0-9]+.*PROD)(.+)", "\\2", relative_path)

dta_files <- fs::path_wd(relative_path)

for (i in seq_along(dta_files)) {
  x   <- dta_files[[i]]
  ver <- versions[[i]]

  command <-
    glue('
        use {x}, clear
        char _dta[version] {ver}
        label var country_code     "country/economy code"
        label var year             "year"
        label var reporting_level  "reporting data level: rural, urban, or national coverage"
        label var welfare_type     "income or consumption used in survey"
        label var percentile       "percentile rank, 1-poorest, 100-richest"
        label var avg_welfare      "average per capita daily consumption or income for the percentile group in PPP \\$"
        label var pop_share        "share of population in percentile group"
        label var welfare_share    "share of income held by each percentile group"
        label var quantile         "upper threshold of income of percentile group"
       save, replace
       ')

  stata(command, stata.echo = FALSE)

}
