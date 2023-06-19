summary_tables = function(combined_federal_states = NA){
  blid_non_list_reason = combined_federal_states |>  
    tabyl(blid, non_list_reason) |> 
    adorn_percentages("row") |> 
    adorn_pct_formatting(digits = 2) |> 
    adorn_ns() 
  
  # save to file
  blid_non_list_reason |> 
    kableExtra::htmlTable(rnames = F) |> 
    kableExtra::save_kable("blid_non_list_reason.png")
  
  
  #print(blid_non_list_reason)
  return(blid_non_list_reason)
}

datasummary_skim_numerical = function(combined_federal_states = NA){
  modelsummary::datasummary_skim(
    combined_federal_states,
    type = "numeric",
    histogram = F,
    output = "summary_skim_numeric.png",
    title = "Skim of Repeated Sales - Numerics"
  )
  return(NULL)
}

datasummary_skim_categorical = function(combined_federal_states = NA){
  modelsummary::datasummary_skim(
    combined_federal_states,
    type = "categorical",
    histogram = F,
    output = "summary_skim_cat.png",
    title = "Skim of Repeated Sales - Categorical"
  )
  return(NULL)
}
