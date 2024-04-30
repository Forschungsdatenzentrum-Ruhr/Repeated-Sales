regression_function = function(indepVar, depVar, fixed_effects = NULL, feols_bool = TRUE) {

rhs <- indepVar |> paste(collapse = " + ")
if(!is.null(fixed_effects)){

  if(feols_bool){
    f <- sprintf("%s ~ %s | %s", depVar, rhs, paste0(fixed_effects, collapse = "^")) |>
      as.formula()
  } else {
    f <- sprintf("%s ~ %s + %s", depVar, rhs, paste0(fixed_effects, collapse = "+")) |>
      as.formula()
  }
  
} else {
  f <- sprintf("%s ~ %s", depVar, rhs) |>
    as.formula()
}

return(f)  
}

