make_hedonic <- function(prepared_hedonic, data_type = NA) {
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
  #pindex = (exp(predict(hedonic_coef, prepared_hedonic))-1)*100
  pindex = mean(hedonic_coef$sumFE)
  out = copy(prepared_hedonic)[, index := pindex]

  return(out)
}
# repeated_index |> group_by(i_type) |> summarise(a = mean(as.numeric(index)), b = mean(as.numeric(index), na.rm = T))
# summary(repeated_index)
