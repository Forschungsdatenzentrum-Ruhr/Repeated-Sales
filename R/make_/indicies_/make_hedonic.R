make_hedonic <- function(prepared_hedonic, data_type) {
  # centralized way of declaring the variables to make adjustments easier
  list_var = make_var(data_type = data_type)
  depVar = list_var$depVar
  indepVar = list_var$indepVar
  fixed_effects = list_var$fixed_effects

  hedonic_coef = feols_regression(
    RED_data = prepared_hedonic, 
    indepVar = indepVar, 
    depVar = depVar, 
    fixed_effects = fixed_effects
  )

  pindex = hedonic_coef$sumFE
  removed_ids = hedonic_coef$obs_selection$obsRemoved

  out = copy(prepared_hedonic)[removed_ids][, index := pindex]

  return(out)
}

