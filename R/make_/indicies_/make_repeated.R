make_repeated <- function(prepared_repeated, grouping_var) {
  
  

  # Berlin plotting fix?
  prepared_repeated <- prepared_repeated[, c(grouping_var) := as.character(get(grouping_var))]
  # taken from rsmatrix vignette
  # see also ?rs_matrix
  matrices <- with(
    prepared_repeated,
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
  grs_b <- qr.coef(qr(Z), y)##
  
  names_grs_b <- names(grs_b)
  
  GRS <- (exp(grs_b) * 100)
  GRS[GRS<0 | GRS > 10000] = NA_integer_

  # GRS_vcov <- rs_var(y - Z %*% grs_b, Z) |>
  #   diag() |>
  #   sqrt()

  # append GRS to data.table
  dt_GRS <- data.table(date_quarter = names_grs_b, i_type = "GRS", index = GRS)

  # Calculate the ARS index in Shiller (1991)
  ars_b <- qr.coef(
    qr(t(Z) %*% X),
    t(Z) %*% Y
  )
  names_ars_b = rownames(ars_b)
  # NA negative values -> why do these only occur in 14612000? 
  # these just become 100 when exp() since they are almost 0
  
  
  #100/
  ARS <- (100/ars_b) |> as.vector()
  ARS[ARS<0 | ARS > 10000] = NA_integer_
  
  
  # vcov <- rs_var(Y - X %*% ars_b, Z, X) |>
  #   diag() |>
  #   sqrt()
  # ARS_vcov <- vcov * ARS^2

  # append ARS to data.table
  dt_ARS <- data.table(date_quarter = names_ars_b, i_type = "ARS", index = ARS)

  # combined repeated indices
  repeated_indices <- rbind(dt_ARS, dt_GRS)

  #--------------------------------s
  return(repeated_indices)
}
