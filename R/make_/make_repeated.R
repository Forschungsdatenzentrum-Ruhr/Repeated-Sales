make_repeated <- function(self_merged_rs_pairs = NA) {

  # taken from rsmatrix vignette
  # see also ?rs_matrix
  matrices <- with(
    # na.omit here since update joins ignore nomatch = 0 argument
    self_merged_rs_pairs,
    rs_matrix(
      t2 = date,
      t1 = prev_date,
      p2 = price_var,
      p1 = prev_price_var,
      f = kid2019, # grouping variable
      sparse = F
    )
  )
  
  # Prep --------------------------------------------------------------------

  Z <- matrices("Z")
  y <- matrices("y")
  X <- matrices("X")
  Y <- matrices("Y")

  # GRS ---------------------------------------------------------------------
  # index via Bailey(1963)
  grs_b <- qr.coef(qr(Z), y) |> na.omit()

  GRS <- (exp(grs_b) * 100) |> formatC(format = "f", digits = 4)
  
  dt_GRS = data.table(date = names(GRS), GRS = GRS)
  # GRS_vcov <- rs_var(y - Z %*% b, Z) |>
  #   diag() |>
  #   sqrt()

  # ARS ---------------------------------------------------------------------

  # index via Shiller (1991)
  ars_b <- qr.coef(
    qr(t(Z) %*% X),
    t(Z) %*% Y
  ) |> na.omit()
  
  # this helps the divide by zero issue but is still kinda weird
  # doesnt fix the weird values
  ars_b[ars_b == 0] = NA
  
  ARS = (100 / ars_b)|> formatC(format = "f", digits = 4)
  dt_ARS = data.table(date = rownames(ars_b),ars_b = ars_b, ARS = ARS)
  
  # vcov <- rs_var(Y - X %*% ars_b, Z, X) |>
  #   diag() |>
  #   sqrt()
  # ARS_vcov <- vcov * ARS^2
  
  
# combined ----------------------------------------------------------------
  repeated_indices = dt_GRS[dt_ARS, on = "date"] |>  drop_na()

  return(repeated_indices)
}
