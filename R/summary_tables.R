custom_cross_tabyl = function(combined_federal_states = NA, arg1 = NA, arg2 = NA){
  
  cross_tabyl = combined_federal_states |> 
    tabyl(!!sym(arg1), !!sym(arg2)) |> 
    adorn_totals("row") |> 
    adorn_percentages("row") |> 
    adorn_pct_formatting(digits = 2) |> 
    adorn_ns() 
  
  # save to file
  cross_tabyl |> 
    htmlTable(rnames = F) |> 
    kableExtra::save_kable(paste0(output_path,"/",arg1,"_",arg2,".png"))
  
  return(cross_tabyl)
}

datasummary_skim_numerical = function(combined_federal_states = NA){
  
  modelsummary::datasummary_skim(
    combined_federal_states,
    type = "numeric",
    histogram = F,
    output = paste0(output_path,"/summary_skim_numeric.png"),
    title = "Skim of Repeated Sales - Numerics"
  )
  return(NULL)
}

datasummary_skim_categorical = function(combined_federal_states = NA){
  
  modelsummary::datasummary_skim(
    combined_federal_states,
    type = "categorical",
    histogram = F,
    output = paste0(output_path,"/summary_skim_cat.png"),
    title = "Skim of Repeated Sales - Categorical"
  )
  return(NULL)
}

custom_threeway_tabyl = function(combined_federal_states = NA, arg1 = NA, arg2 = NA, arg3 = NA){
  
  cross_tabyl = combined_federal_states |> 
    tabyl(!!sym(arg1), !!sym(arg2), !!sym(arg3)) |> 
    adorn_totals("row") |> 
    adorn_percentages("row") |> 
    adorn_pct_formatting(digits = 2) |> 
    adorn_ns() 
  
  # save to file
  cross_tabyl[[1]] |> 
    htmlTable(rnames = F) |> 
    kableExtra::save_kable(paste0(output_path,"/",arg1,"_",arg2,"_",arg3,".png"))
  
  return(cross_tabyl)
}
