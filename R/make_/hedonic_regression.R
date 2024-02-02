hedonic_regression = function(RED_data = NA,indepVar = NA, depVar = NA, fixed_effects= NA ){
  
  tar_assert_true(all(indepVar %in% names(RED_data)))
  
  rhs <- indepVar |> paste(collapse = " + ")
  f <- sprintf("%s ~ %s | %s", depVar, rhs, paste0(fixed_effects, collapse = "^")) |>
    as.formula()
  
  hedonic <- feols(f, RED_data, combine.quick = F, mem.clean = T)
  #fe = fixef(hedonic)
  
  return(hedonic)
}

