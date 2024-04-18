make_hedonic <- function(prepared_hedonic, data_type) {
  # centralized way of declaring the variables to make adjustments easier
  list_var <- make_var_list(data_type = data_type)
  depVar <- list_var$depVar
  indepVar <- list_var$indepVar
  fixed_effects <- list_var$fixed_effects

  hedonic_coef <- feols_regression(
    RED_data = prepared_hedonic,
    indepVar = indepVar,
    depVar = depVar,
    fixed_effects = fixed_effects
  )
  # really feel like this should be possible with data.table but cant find it
  removed_ids <- hedonic_coef$obs_selection$obsRemoved
  pindex <- (exp(hedonic_coef$sumFE) - 1) * 100
  # drop ids that were removed during regression and assign index
  if (length(removed_ids) > 0) {
    prepared_hedonic <- dplyr::slice(prepared_hedonic, removed_ids * -1)
  }
  out <- prepared_hedonic[, index := pindex]

  return(out)
}
