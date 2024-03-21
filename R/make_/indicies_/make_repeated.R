make_repeated <- function(prepared_repeated, grouping_var) {

  # bandaid to filter top bottom half a percent of pice variables to catch incredible outliers
  upper_percentile <- quantile(prepared_repeated[price_var >= 0, price_var], 1 - (0.1 / 100))
  lower_percentile <- quantile(prepared_repeated[price_var >= 0, price_var], (0.1 / 100))
  
  # taken from rsmatrix vignette
  # see also ?rs_matrix
  matrices <- with(
    prepared_repeated[
      price_var %between% c(lower_percentile, upper_percentile)
    ],
    rs_matrix(
      t2 = date_quarter,
      t1 = prev_date_quarter,
      p2 = price_var,
      p1 = prev_price_var,
      f = eval(as.symbol(grouping_var)),
      sparse = F
    )
  )
  
  # Prep 
  Z <- matrices("Z")
  y <- matrices("y")
  X <- matrices("X")
  Y <- matrices("Y")

  # Calculate the GRS index in Bailey, Muth, and Nourse (1963) 
  grs_b <- qr.coef(qr(Z), y)
  GRS <- (exp(grs_b) * 100) 

  # GRS_vcov <- rs_var(y - Z %*% grs_b, Z) |>
  #   diag() |>
  #   sqrt()

  # append GRS to data.table
  dt_GRS = data.table(date_quarter = names(GRS), i_type = "GRS", index = GRS |> formatC(format = "f", digits = 4))

  # Calculate the ARS index in Shiller (1991)
  ars_b <- qr.coef(
    qr(t(Z) %*% X),
    t(Z) %*% Y
  )
  ARS = (100 / ars_b) |> as.vector()

  # vcov <- rs_var(Y - X %*% ars_b, Z, X) |>
  #   diag() |>
  #   sqrt()
  # ARS_vcov <- vcov * ARS^2

  # append ARS to data.table
  dt_ARS = data.table(date_quarter = rownames(ars_b), i_type = "ARS", index = ARS |> formatC(format = "f", digits = 4)) 
 
  # combined repeated indices
  repeated_indices = rbind(dt_ARS,dt_GRS)

  #--------------------------------s
  return(repeated_indices)
}

