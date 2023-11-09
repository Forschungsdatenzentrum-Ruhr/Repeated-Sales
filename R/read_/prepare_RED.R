prepare_RED <- function(RED_all_columns = NA, var_of_interest = NA) {

  RED_req_columns <- RED_all_columns[
    # drop missings for all but pricing variables since one of those is always missing
    # drops like 8-10 mil
    RED_all_columns[,
      Reduce("&", lapply(.SD, ">=", 0)),
      # this is prob unnecessary
      .SDcols = var_of_interest
    ],
    # keep only relevant variables
    ..var_of_interest
    # unique only
    # drops like 2mil
  ][price_var >0] |> unique()


  return(RED_req_columns)
}
