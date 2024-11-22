make_improved_hybrid = function(RED_subset_classified, prepared_repeated, data_type){
  #' @title Make Imporved Hybrid Index
  #'
  #' @description Make the improved hybrid index for the given data type
  #' @param RED_subset_classified data.table. Classified RED data
  #' @param prepared_repeated data.table. Prepared repeated data
  #' @param repeated_index data.table. Complete repeated index
  #' @param data_type character. Data type of the classified RED data
  #'
  #' @note Build by me based on Englund et.al 1996. Improved Price Indexes for Real Estate: Measuring the Course of Swedish Housing Prices
  #'
  #' @return data.table. Hybrid index for the given data type
  #' @author Thorben Wiebe
  #----------------------------------------------
  # Input validation
  input_check(RED_subset_classified, "data.table")
  input_check(prepared_repeated, "data.table")
  input_check(data_type, "character")
  #----------------------------------------------
  # prep, get some settings
  list_var <- make_var_list(data_type = data_type)
  depVar <- list_var$depVar
  indepVar <- list_var$indepVar
  # drop baujahr from indepVar since its used as YR
  indepVar = setdiff(indepVar, "baujahr")

  # mutate variables in original data
  RED_subset_classified <- prepare_hedonic(RED_subset_classified, data_type)
  RED_subset_classified[, ":="(
    # this is still logged
    depVar = get(depVar),
    YR = baujahr,
    AGE = baujahr - ejahr
  )]

  # FIRST STEP -> REPEATED SALES INDEX

  # get ids of all listings that are classified as repeat sales (pure or changed)
  all_rs <- prepared_repeated[["rs_id"]] |> unique()
  RED_rs = RED_subset_classified[rs_id %in% all_rs]

  # make repeated index for all of Germany (P_t = sum o_j)
  # everything is one group since we only care for quarters, not spatial
  grouping_dummy = "Germany"
  prepared_repeated$grouping_dummy = grouping_dummy
  repeated_index = make_repeated(prepared_repeated, grouping_var = "grouping_dummy")
  # subset to GRS and retransform quarter
  repeated_index = repeated_index[i_type == "GRS", 
  .(
    date_quarter = stringr::str_replace_all(date_quarter, glue::glue("^{grouping_dummy}."), ""),
    index
  )
  ]

  # make time dummy for all dates (D_j)
  D = make_D(
    RED_rs$date_month, 
    start_period = min(RED_rs$date_month), 
    end_period = max(RED_rs$date_month)
  )

  tar_assert_true(nrow(repeated_index) == ncol(D), "Number of quarters does not match number of GRS quarters")

  # step one -> equation 6 via repeated sales alone using OLS
  # equation: V_it= b*X*_it + b_y YR_i + sum o_j D_j + E_i + e_it
  P_t = sweep(D, 2, repeated_index$index, "*") |> rowSums() # swap for %+%?

  equ_6 = lm(depVar ~ indepVar + YR + P_t - 1, data = RED_rs)

  o_e = equ_6$residual

  # V_it = bX_it + P_t + E_i + e_it
  
  # = b*X*_it + b_y YR_i + b_d AGE_it + sum o_j D_j + E_i + e_it
  # YR_i = building year
  # AGE = depreciation age
  # b_v = b_y - b_d

  
  
  
}

make_D = function(dates, start_period, end_period){
  
  # convert dates to quarters
  dates = zoo::as.yearqtr(dates)
  start_period = zoo::as.yearqtr(start_period)
  end_period = zoo::as.yearqtr(end_period)

  # difference between dates in quarters
  total_quarters = as.numeric(end_period - start_period) * 4
  date_to_start = sapply(dates, function(x) as.numeric(x - start_period) * 4)
  date_to_end = sapply(dates, function(x) as.numeric(end_period - x) * 4)

  # create D matrix
  D = matrix(0, nrow = length(dates), ncol = total_quarters)
  # update prior quarters to 1
  # consider setting the actual selling quarter to 0.5? 
  for(i in seq_along(dates)){
    # this just cuts off the last quarter if its not a full quarter yet
    j = min(date_to_start[i] + 1, total_quarters)
    pre_date = seq(1, j ,1)
    D[i, pre_date] = 1
  }
  
  tar_assert_true(length(dates) == nrow(D), "Length of dates does not match D")
  
  return(D)
  
}
