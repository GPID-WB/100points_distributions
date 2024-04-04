dt[,
   # get bins and total pop and welfare
   `:=`(
     bin = wbpip:::md_compute_bins(welfare_ppp,
                                   weight,
                                   nbins = nq,
                                   output = "simple"),
     tot_pop = sum(weight),
     tot_wlf = sum(welfare_ppp*weight)
   ),
   by = c("imputation_id", "reporting_level")
]


dt[,
   # get bins and total pop and welfare

     bin := wbpip:::md_compute_bins(welfare_ppp,
                                   weight,
                                   nbins = nq,
                                   output = "simple"),
   by = c("imputation_id", "reporting_level")
   ][,
     `:=`(
       tot_pop = sum(weight),
       tot_wlf = sum(welfare_ppp*weight)
     ),
     by = c("imputation_id", "reporting_level")
     ]

w <- dt$weight
g <- GRP(dt, c("imputation_id", "reporting_level"))

dt |>
  fgroup_by(imputation_id, reporting_level) |>
  fmutate(tot_pop = fsum(weight),
          tot_wlf = fsum(welfare_ppp*weight),
          bin = wbpip:::md_compute_bins(welfare_ppp,
                                        weight = w,
                                        nbins = nq,
                                        output = "simple"))



