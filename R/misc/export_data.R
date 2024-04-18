export_data <- function(classification, data_type) {
  #' @title Export data
  #'
  #' @description Export data to file.
  #' @param classification data.table. Classification data.
  #' @param data_type character. Type of data to be used. One of:
  #' *WK = Wohnungskauf/ Apartments for sale
  #' *WM = Wohnungsmiete/ Apartments for rent
  #' *HK = Hauskauf/ Houses for sale
  #'
  #' @return WIP
  #' @author Thorben Wiebe
  #----------------------------------------------
  # Input validation
  input_check(classification, "data.table")
  input_check(data_type, "character")
  tar_assert_true(data_type %in% c("WK", "WM", "HK"), "data_type must be one of WK, WM, HK")
  #----------------------------------------------
  # paste path together
  final_path = glue::glue("{output_path}/RED_RSR_{data_type}_{RED_version}.fst")
  
  classification |>
      # write to file
     fst::write.fst(
      path = final_path
    )
  #----------------------------------------------
  # Unit test
  tar_assert_true(file.exists(final_path), "File does not exist.")
  #----------------------------------------------
  return(NULL)
}
