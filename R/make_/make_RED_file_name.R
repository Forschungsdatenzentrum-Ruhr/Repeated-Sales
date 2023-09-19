make_RED_file_name <- function(data_version = NA, data_type = c("Wk", "Wm")) {
  #' @title Creation of Filename for specified real estate data (RED)
  #'
  #' @description constructs file name (full file path) of the file to
  #' be used by pipeline
  #' @param data_version version of data to be used. Typically in form "vX" where
  #' 'X' is the wave numbering
  #' @param data_type data_type of data to be used. One of:
  #' *wk = Wohnungskauf/ Apartments for sale
  #' *wm = Wohnungsmiete/ Apartments for rent
  #'
  #' @note
  #'
  #' @return A string containing the full file path of the requested file
  #' @author Thorben Wiebe 
  #----------------------------------------------
 
  # construct file path of specified data version
  path <- paste0("M:/_FDZ/RWI-GEO/RWI-GEO-RED/daten/On-site", "/", data_version, "/")

  # construct full file path based on path and file name
  #prior: "_allVersionsLabels.dta"
  file_name <- paste0(path, data_type, "_allVersions_ohneText.dta")


  return(file_name)
}

