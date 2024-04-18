prepare_hedonic <- function(RED_classified, data_type) {
  #' @title Prepare hedonic
  #' 
  #' @description This function prepares the hedonic index by data type.
  #' @param RED_classified data.table. RED classified data.
  #' @param data_type character. Type of data to be used. One of:
  #' *WK = Wohnungskauf/ Apartments for sale
  #' *WM = Wohnungsmiete/ Apartments for rent
  #' *HK = Hauskauf/ Houses for sale
  #' 
  #' @return data.table. Prepared hedonic index.
  #' @author Thorben Wiebe
  #----------------------------------------------
  # Input validation
  input_check(RED_classified, "data.table")
  input_check(data_type, "character")
  tar_assert_true(data_type %in% c("WK", "WM", "HK"))
  #---------------------------------------------
  if (data_type == "WK") {
    prepared_hedonic <- make_hedonic_WK(RED_classified)
  } else if (data_type == "WM") {
    prepared_hedonic <- make_hedonic_WM(RED_classified)
  } else if (data_type == "HK") {
    prepared_hedonic <- make_hedonic_HK(RED_classified)
  }
  #----------------------------------------------
  # Unit test
  empty_check(prepared_hedonic)
  #----------------------------------------------
  return(prepared_hedonic)
}