custom_cross_tabyl <- function(data, arg1, arg2) {
  cross_tabyl <- data |>
    tabyl(!!sym(arg1), !!sym(arg2)) |>
    adorn_totals("row") |>
    adorn_percentages("row") |>
    adorn_pct_formatting(digits = 2) |>
    adorn_ns()

  # save to file
  cross_tabyl |>
    htmlTable(rnames = F) |>
    kableExtra::save_kable(paste0(output_path, "/", arg1, "_", arg2, ".png"))

  return(cross_tabyl)
}

datasummary_skim_numerical <- function(data) {
  modelsummary::datasummary_skim(
    data,
    type = "numeric",
    histogram = F,
    output = paste0(output_path, "/summary_skim_numeric.png"),
    title = "Skim of Repeated Sales - Numerics"
  )
  return(NULL)
}

datasummary_skim_categorical <- function(data) {
  modelsummary::datasummary_skim(
    data,
    type = "categorical",
    histogram = F,
    output = paste0(output_path, "/summary_skim_cat.png"),
    title = "Skim of Repeated Sales - Categorical"
  )
  return(NULL)
}

custom_threeway_tabyl <- function(data, arg1, arg2, arg3) {
  cross_tabyl <- data |>
    tabyl(!!sym(arg1), !!sym(arg2), !!sym(arg3)) |>
    adorn_totals("row") |>
    adorn_percentages("row") |>
    adorn_pct_formatting(digits = 2) |>
    adorn_ns()

  # save to file
  cross_tabyl[[1]] |>
    htmlTable(rnames = F) |>
    kableExtra::save_kable(paste0(output_path, "/", arg1, "_", arg2, "_", arg3, ".png"))

  return(cross_tabyl)
}

  
  
  


