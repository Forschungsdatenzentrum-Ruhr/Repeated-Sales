feols_regression = function(RED_data,indepVar, depVar, fixed_effects){
  #' @title WIP
  #'
  #' @description WIP
  #' @param WIP
  #' @param WIP
  #' @note
  #'
  #' @return WIP
  #' @author Thorben Wiebe
  #----------------------------------------------

  # Unit test
  # check if all names made it into the data
  tar_assert_true(all(indepVar %in% names(RED_data)), msg = paste0("Missing: ", setdiff(indepVar, names(RED_data))))

  # check for NAs that got through cleaning
  NA_check = RED_data[,lapply(.SD, function(x){anyNA(x)}), .SDcols = c(indepVar,fixed_effects)] |> unlist()
  tar_assert_true(!any(NA_check), msg = names(NA_check)[NA_check], msg = "NAs in data")
  
  # construct regression formula
  rhs <- indepVar |> paste(collapse = " + ")
  f <- sprintf("%s ~ %s | %s", depVar, rhs, paste0(fixed_effects, collapse = "^")) |>
    as.formula()
  
  # run regression
  hedonic <- feols(f, RED_data, combine.quick = F, mem.clean = T)
  
  #----------------------------------------------
  return(hedonic)
}

