make_repeated <- function(prepared_repeated, grouping_var) {
  #' @title Make Repeated Sales Index
  #' 
  #' @description Make the repeated sales index for the given data type
  #' @param prepared_repeated data.table. Prepared repeated sales data
  #' @param grouping_var character. Variable to group by
  #' 
  #' @return data.table. Repeated sales index for the given data type
  #' @author Thorben Wiebe
  #----------------------------------------------
  # Input validation
  input_check(prepared_repeated, "data.table")
  input_check(grouping_var, "character")
  #----------------------------------------------
  # prep
  prepared_repeated <- prepared_repeated[, c(grouping_var) := as.character(get(grouping_var))]
  # NOTE: taken from rsmatrix vignette -> see also ?rs_matrix
  # create matrices
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

  # drag out matrices for easier access
  Z <- matrices("Z")
  y <- matrices("y")
  X <- matrices("X")
  Y <- matrices("Y")

  # Calculate the GRS index in Bailey, Muth, and Nourse (1963)
  grs_b <- qr.coef(qr(Z), y)##
  
  names_grs_b <- names(grs_b)
  
  GRS <- (exp(grs_b) * 100)
  # bandaid solution for negative values and heavy outliers
  GRS[GRS<0 | GRS > 10000] = NA_integer_

  # delta method for standard errors
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
  
  ARS <- (100/ars_b) |> as.vector()
  # bandaid solution for negative values and heavy outliers
  ARS[ARS<0 | ARS > 10000] = NA_integer_
  
  # delta method for standard errors
  # vcov <- rs_var(Y - X %*% ars_b, Z, X) |>
  #   diag() |>
  #   sqrt()
  # ARS_vcov <- vcov * ARS^2

  # append ARS to data.table
  dt_ARS <- data.table(date_quarter = names_ars_b, i_type = "ARS", index = ARS)

  # combined repeated indices
  repeated_indices <- rbind(dt_ARS, dt_GRS)

  #--------------------------------
  # Unit test
  empty_check(repeated_indices)
  #--------------------------------
  return(repeated_indices)
}
