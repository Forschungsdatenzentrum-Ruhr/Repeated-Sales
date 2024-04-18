prepare_RED <- function(RED_full_data, var_of_interest) {
  #' @title Prepare RED data for analysis
  #'
  #' @description Prepare RED data for analysis by dropping irrelevant variables and
  #' missing values.
  #' @param RED_full_data data.table. Full RED data set.
  #' @param var_of_interest character. Vector of variable names to be kept in the data set.
  #'
  #' @return data.table. RED data set with relevant variables and no missing values.
  #' @author Thorben Wiebe
  # ----------------------------------------------
  # Input validation
  input_check(RED_full_data, "data.table")
  input_check(var_of_interest, "character")
  #----------------------------------------------

  # replace missing values with 0 for variables where missing is the same as 0
  var_to_replace_missings <- c("balkon")
  RED_full_data[,
    (var_to_replace_missings) := lapply(.SD, function(x) {
      fifelse(x < 0, 0, x)
    }),
    .SDcols = var_to_replace_missings
  ]

  RED_req_columns <- RED_full_data[
    # drop missing values
    RED_full_data[,
      Reduce("&", lapply(.SD, ">=", 0)),
      .SDcols = var_of_interest
    ],
    # keep only relevant variables
    ..var_of_interest
    # make unique and ensure that price_var is positive
  ][price_var > 0] |> unique()

  #----------------------------------------------
  # Unit test
  tar_assert_true(
    all(
      var_of_interest %in% names(RED_req_columns)),
      msg = "Not all variables of interest are present in the data."
  )
  empty_check(RED_req_columns)
  #----------------------------------------------
  return(RED_req_columns)
}
