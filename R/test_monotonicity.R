
## !!! folders parameters: change to desired folder in pip
version <- "20250401_2021_01_02_PROD"

album_dir <-
  fs::path("data/album", version)

singles_dir <-
  fs::path("data/singles", version)


wld <- fs::path(album_dir, "world_100bin.qs") |>
  qs::qread()

# sorting vars
svars <- c("country_code", "reporting_level", "welfare_type", "year", "percentile")

# Grouping vars
gvars <- c("country_code", "reporting_level", "welfare_type", "year")
setorderv(wld,svars)

g <- GRP(wld, gvars)

failing <-
  wld |>
  ftransform(tag = (welfare_share - flag(welfare_share,g = g)) < 0) |>
  fsubset(tag == TRUE) |>
  _[, ..gvars] |>
  unique()

x <- failing[1]
f <- paste(x$country_code, x$year, x$welfare_type, "100bin", sep = "_")

dt <-
  fs::dir_ls(singles_dir, regexp = f) |>
  qs::qread()

f

ori_name <- copy(names(dt))

nq <- 100
# Create a vector of values logarithmically spaced from 1e-15 to 1e-8
small_values <- seq(log(1e-7), log(9e-7),
                    length.out = nq) |>
  exp()
any(diff(small_values) < 0)

nbybins <- dt[, .N, by = reporting_level]

sv <- vector("list", nrow(nbybins))

for (i in seq_len(nrow(nbybins))) {
  nbins   <- nbybins[i, N]
  sv[[i]] <-
    seq(log(1e-7), log(9e-7),
        length.out = nbins) |>
    exp()
}
sv <- unlist(sv)

y <- dt |>
  add_vars(small = sv) |>
  #  1. create equal sized bins
  ftransform(tot_pop = fsum(pop, reporting_level, TRA = "fill"),
             bin_total = fnobs(pop, reporting_level, TRA = "fill")) |>
  ftransform(pop_adj = tot_pop / bin_total) |>
 # 2. create total welfare of each bin and
 # aggregate welfare in the population
  ftransform(tot_welf_orign = avg_welfare * pop,
             tot_welf_equal = avg_welfare * pop_adj) |>
  ftransform(agg_welf_orign_sum = fsum(tot_welf_orign,
                                       reporting_level, TRA = "fill"),
             agg_welf_equal_sum = fsum(tot_welf_equal,
                                       reporting_level, TRA = "fill")) |>
  #  3. generate the discrepancy (residual) of aggregate welfare
  # in population using original or equalized population  distribute
  # the discrepancy using the original distribution of average welfare
  ftransform(residual = agg_welf_orign_sum - agg_welf_equal_sum,
             tot_avg_welf = fsum(avg_welfare, reporting_level,TRA = "fill")
  ) |>
  ftransform(tot_welf_adj =
               tot_welf_equal + small +
               (residual * avg_welfare/tot_avg_welf)) |>
  # first adjust to avg welfare. change name of var to avg_welf_adj
  # to compare
  #. 4. re-create a new average welfare
  ftransform(avg_welfare_adj =  tot_welf_adj/pop_adj) |>
  # Second adjust to welfare share. change name of var to welf_share_adj
  # to compare
  ftransform(welf_share_adj = tot_welf_adj/agg_welf_orign_sum) |>
  ftransform(diff = (welf_share_adj - flag(welf_share_adj,
                                           g = reporting_level))) |>
  # ftransform(tag =  round(diff, digits = 10) < 0)
  ftransform(tag =  diff < 0)


any(y[!is.na(tag), tag] == TRUE)


y[, .(bin, avg_welfare, avg_welfare_adj,  welfare_share, welf_share_adj, tag)]



ago <- wld[country_code == "AGO" & year == 2000]
arg <- wld[country_code == "ARG" & year == 1986]



ago |>
  ftransform(tag = (welfare_share - flag(welfare_share,
                                      g = reporting_level)) < 0)

y |>
  ftransform(tag = (welfare_share - flag(welfare_share,
                                      g = reporting_level)) < 0)


z <- fix_welfare_share(ago, 100)


z |>
  ftransform(tag = (welfare_share - flag(welfare_share,
                                         g = reporting_level)) < 0)






nq <- 100

ct <- "ARG"
ct <- "CHN"

yr <- 2010

df   <- pipload::pip_load_cache(ct, yr,
                                verbose = FALSE,
                                version = version)

setorder(df,
         imputation_id,
         reporting_level,
         welfare_type,
         welfare_ppp,
         weight)

# adjust weights to number of imputations

# number of imputations
n_ids <- df[, uniqueN(imputation_id)]

# welfare type
wt <- funique(df$welfare_type)

df <- df |>
  fselect(reporting_level, welfare = welfare_ppp, weight)

if (n_ids > 1) {
  df[, weight := weight/n_ids]
}


df[, .(reporting_level, welfare, weight)] |>
  fwrite(fs::path(tdirp, "distribution.csv"))




# R  <- duplicate_households(df)
# lt    <- attr(R, "lorenz")
# ws_OK <- attr(R, "welfare_share_OK")
# lt <- lorenz_table(df, nq = 1000) |>
lt <- lorenz_table(df, nq = nq) |>
  # fix welfare share.
  fix_welfare_share(nq = nq)


lt |>
  ftransform(diff = welfare_share - flag(welfare_share,
                                         g = reporting_level) ) |>
  ftransform(tag = diff < 0) |>
  fsubset(tag == TRUE)



arg <- wld[country_code == "ARG" & year == 1997, names(lt)]

waldo::compare(arg, lt)
