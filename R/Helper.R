#' Helper and Debug Functions, WIP-Section
#' 
#' @description
#' Universal functions used during different steps of pipeline aswell as WIP-
#' Section and debugging
#'

tbl_transpose <- function(data_to_transpose, ...) {
  return(as_tibble(t(data_to_transpose), ...))
}

debug_Helper <- function(latlon = "5930063.7517479582065.965598572") {
  test <- tar_read(plz_group_1)
  test <- filter(test, as.character(latlon_utm) == latlon)
  data <<- test
}