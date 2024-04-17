make_RED_file_name <- function(data_version, data_type) {
  #' @title Creation of Filename for specified real estate data (RED)
  #'
  #' @description Constructs file name (full file path) of the file to
  #' be used by pipeline.
  #' @param data_version character. Version of data to be used. Typically in form "vX" where
  #' 'X' is the wave numbering
  #' @param data_type character. Type of data to be used. One of:
  #' *WK = Wohnungskauf/ Apartments for sale
  #' *WM = Wohnungsmiete/ Apartments for rent
  #' *HK = Hauskauf/ Houses for sale
  #'
  #' @return character. Full file path of the specified RED data file.
  #' @author Thorben Wiebe
  #----------------------------------------------
  # Input validation
  tar_assert_true(
    data_type %in% c("WK", "WM", "HK"),
    glue::glue("{data_type} must be one of c('WK', 'WM', 'HK').")
  )
  input_check(data_version, "character")
  #----------------------------------------------

  # construct file path of specified data version
  path <- paste0("M:/_FDZ/RWI-GEO/RWI-GEO-RED/daten/On-site", "/", data_version, "/")

  # construct full file path based on path and file name
  file_name <- paste0(path, data_type, "_allVersions_ohneText.dta")
  
  #----------------------------------------------
  # Unit test
  tar_assert_true(
    file.exists(file_name),
    glue::glue("File {file_name} does not exist.")
  )
  #----------------------------------------------
  return(file_name)
}
