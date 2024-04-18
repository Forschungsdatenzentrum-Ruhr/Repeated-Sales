make_hedonic <- function(prepared_hedonic, data_type) {
  #' @title Make Hedonic Index
  #' 
  #' @description Make the hedonic index for the given data type
  #' @param prepared_hedonic data.table. Prepared hedonic data
  #' @param data_type character. Data type of the prepared hedonic data
  #' 
  #' @return data.table. Hedonic index for the given data type
  #' @author Thorben Wiebe
  #----------------------------------------------
  # Input validation
  input_check(prepared_hedonic, "data.table")
  input_check(data_type, "character")
  #----------------------------------------------

  # centralized way of declaring the variables to make adjustments easier
  list_var <- make_var_list(data_type = data_type)
  depVar <- list_var$depVar
  indepVar <- list_var$indepVar
  fixed_effects <- list_var$fixed_effects
  # run regression
  hedonic_coef <- feols_regression(
    RED_data = prepared_hedonic,
    indepVar = indepVar,
    depVar = depVar,
    fixed_effects = fixed_effects
  )
  # NOTE: really feel like this should be possible with data.table but cant find it
  # drop ids that were removed during regression and assign index
  removed_ids <- hedonic_coef$obs_selection$obsRemoved
  pindex <- (exp(hedonic_coef$sumFE) - 1) * 100
  if (length(removed_ids) > 0) {
    prepared_hedonic <- dplyr::slice(prepared_hedonic, removed_ids * -1)
  }
  out <- prepared_hedonic[, index := pindex]
  #----------------------------------------------
  # Unit test
  empty_check(out)
  #----------------------------------------------
  return(out)
}
