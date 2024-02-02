make_repeated <- function(self_merged_rs_pairs, grouping_var) {
  # add date column here -> might be time to make this a class as well ?

  # bandaid to filter top bottom half a percent of pice variables to catch incredible outliers
  # there are still NAs in ARS -> accept or fix
  #upper_percentile <- quantile(self_merged_rs_pairs[price_var >= 0, price_var], 1 - (0.1 / 100))
  #lower_percentile <- quantile(self_merged_rs_pairs[price_var >= 0, price_var], (0.1 / 100))
  
  # taken from rsmatrix vignette
  # see also ?rs_matrix
  # self_merged_rs_pairs = self_merged_rs_pairs[
  #     price_var %between% c(lower_percentile, upper_percentile) &
  #     kid2019 %in% c("1054")
  #   ]
  
  matrices <- with(
    self_merged_rs_pairs
      #!price_var == prev_price_var &
      #price_var %between% c(lower_percentile, upper_percentile)
    ,
    rs_matrix(
      t2 = date_quarter,
      t1 = prev_date_quarter,
      p2 = price_var,
      p1 = prev_price_var,
      f = eval(as.symbol(grouping_var)),
      sparse = F
    )
  )
  
  # Prep --------------------------------------------------------------------
  # TODO: make the cutting a function and equal for all -> do it after merging?
  Z <- matrices("Z")
  y <- matrices("y")
  X <- matrices("X")
  Y <- matrices("Y")

  # GRS ---------------------------------------------------------------------
  # index via Bailey(1963)
  #grs_b <- qr.coef(qr(Z), y) #|> na.omit()
  grs_b = lm(y ~ Z)
  GRS = (exp(predict(grs_b, Z))-1)*100

  # Calculate the GRS index in Bailey, Muth, and Nourse (1963) ss
  GRS_old <- (exp(grs_b) * 100) 
  print(identical(GRS, GRS_old))

  # GRS_vcov <- rs_var(y - Z %*% grs_b, Z) |>
  #   diag() |>
  #   sqrt()


  
  dt_GRS = data.table(date_quarter = names(GRS), i_type = "GRS", index = GRS |> formatC(format = "f", digits = 4))

  # ARS ---------------------------------------------------------------------

  # index via Shiller (1991)
  ars_b <- qr.coef(
    qr(t(Z) %*% X),
    t(Z) %*% Y
  ) #|> na.omit()
  
  # this helps the divide by zero issue but is still kinda weird
  # doesnt fix the weird values
  # ars_b[ars_b == 0] = NA
  
  ARS = (100 / ars_b) |> as.vector()

  # vcov <- rs_var(Y - X %*% ars_b, Z, X) |>
  #   diag() |>
  #   sqrt()
  # ARS_vcov <- vcov * ARS^2

  dt_ARS = data.table(date_quarter = rownames(ars_b), i_type = "ARS", index = ARS |> formatC(format = "f", digits = 4)) 
 
# combined ----------------------------------------------------------------
  #repeated_indices = dt_GRS[.(dt_ARS), on = "date_quarter"] |>  drop_na()
  repeated_indices = rbind(dt_ARS,dt_GRS)

  return(repeated_indices)
}

