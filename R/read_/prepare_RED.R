prepare_RED <- function(RED_all_columns = NA, var_of_interest = NA) {

  
  # initial_obs = RED_req_columns[,.N]
  # logger::log_info("Initial Observations: ", initial_obs)
  
  RED_req_columns <- RED_all_columns[
  # drop missings for all but pricing variables since one of those is always missing
  # drops like 8-10 mil
    RED_all_columns[,
      Reduce("&", lapply(.SD, ">=", 0)),
      # this is prob unnecessary
      #.SDcols = setdiff(var_of_interest, c("mietekalt", "kaufpreis"))
      .SDcols = var_of_interest
  ],
  # keep only relevant variables
  ..var_of_interest
  # unique only
  # drops like 2mil
] |> unique()


# Unit-test
#check_nonsensical_listings(RED_req_columns,"RED_req_columns")

# logger::log_info("Post Reading Observations: ", RED_req_columns[,.N])
  return(RED_req_columns)

}