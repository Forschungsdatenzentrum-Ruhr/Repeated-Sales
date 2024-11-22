feols_regression = function(RED_data,indepVar, depVar, fixed_effects){
  #' @title Fixed Effects OLS Regression
  #'
  #' @description Run a fixed effects OLS regression on the data
  #' @param RED_data data.table. Data to run regression on
  #' @param indepVar character. Independent variables
  #' @param depVar character. Dependent variable
  #' @param fixed_effects character. Fixed effects
  #'
  #' @return object. Fixed effects OLS regression object
  #' @author Thorben Wiebe
  #----------------------------------------------
  # Input validation
  input_check(RED_data, "data.table")
  input_check(indepVar, "character")
  input_check(depVar, "character")
  input_check(fixed_effects, "character")
  #----------------------------------------------

  # Unit test: check if all variables are in the data
  all_vars = c(indepVar,depVar,fixed_effects)
  tar_assert_true(all(all_vars %in% names(RED_data)), msg = paste0("Missing: ", setdiff(all_vars, names(RED_data))))

  # Unit test: check for NAs that got through cleaning
  NA_check = RED_data[,lapply(.SD, function(x){anyNA(x)}), .SDcols = all_vars] |> unlist()
  tar_assert_true(!any(NA_check), msg = glue::glue("NAs in data, columns: {names(NA_check)[NA_check]}"))
  
  # construct regression formula
  f = regression_function(indepVar, depVar, fixed_effects)
  
  # run regression
  hedonic <- feols(f, RED_data, combine.quick = F, mem.clean = T)
  # ----------------------------------------------
  # Unit test
  tar_assert_true(!is.null(hedonic), msg = "Fixed effects regression failed")
  #----------------------------------------------
  return(hedonic)
}

