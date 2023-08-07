custom_cross_tabyl <- function(combined_federal_states = NA, arg1 = NA, arg2 = NA) {
  cross_tabyl <- combined_federal_states |>
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

datasummary_skim_numerical <- function(combined_federal_states = NA) {
  modelsummary::datasummary_skim(
    combined_federal_states,
    type = "numeric",
    histogram = F,
    output = paste0(output_path, "/summary_skim_numeric.png"),
    title = "Skim of Repeated Sales - Numerics"
  )
  return(NULL)
}

datasummary_skim_categorical <- function(combined_federal_states = NA) {
  modelsummary::datasummary_skim(
    combined_federal_states,
    type = "categorical",
    histogram = F,
    output = paste0(output_path, "/summary_skim_cat.png"),
    title = "Skim of Repeated Sales - Categorical"
  )
  return(NULL)
}

custom_threeway_tabyl <- function(combined_federal_states = NA, arg1 = NA, arg2 = NA, arg3 = NA) {
  cross_tabyl <- combined_federal_states |>
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
check_nonsensical_listings <- function(data_connected = NA, data_name = NA) {
  # calc ayear/eyear from amonths/emonths
  data_connected[, ":="(ayear = amonths %/% 12, eyear = emonths %/% 12)]

  # table ayear vs eyear and remove name column
  ayear_eyear_table <- data_connected |>
    tabyl(ayear, eyear) |>
    select(-any_of(c("ayear", "eyear"))) |>
    as.matrix()


  # save to file
  ayear_eyear_table |>
    htmlTable(rnames = F) |>
    kableExtra::save_kable(paste0(output_path, "/", data_name, "_", "ayear", "_", "eyear", ".png"))

  # set upper triangle of matrix including diag as NA (these are okay to be >0)
  ayear_eyear_table[upper.tri(ayear_eyear_table, diag = T)] <- NA

  # calc sum of colsums to check that entire lower triangle of matrix is contains only 0
  tar_assert_true(
    sum(colSums(ayear_eyear_table, na.rm = T)) == 0,
    msg = glue::glue("nonsensical ayear/eyear combinations found at: {unique(data_connected$latlon_utm)}")
  )
}
