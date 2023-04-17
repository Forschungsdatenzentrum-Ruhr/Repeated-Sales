#' Constructing of file name (full file path)

#' @description
#' 'construct_file_name' constructs file name (full file path) of the file to
#' be used by pipeline
#'
#' @return
#' A string containing the full file path of the requested file
#'
#' @param data_version version of data to be used. Typically in form "vX" where
#' 'X' is the wave numbering
#' @param data_type data_type of data to be used. One of:
#' *wk = Wohnungskauf/ Apartments for sale
#' *wm = Wohnungsmiete/ Apartments for rent
#'
#'
construct_file_name <- function(data_version = NA, data_type = c("Wk", "Wm")) {

  # construct file path of specified data version
  path <- paste0("M:/_FDZ/RWI-GEO/RWI-GEO-RED/daten/On-site", "/", data_version, "/")

  # construct full file path based on path and file name
  file_name <- paste0(path, data_type, "_allVersionsLabels.dta")


  return(file_name)
}
