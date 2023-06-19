#' Helper and Debug Functions, WIP-Section
#' 
#' @description
#' Universal functions used during different steps of pipeline aswell as WIP-
#' Section and debugging
#'

tbl_transpose <- function(data_to_transpose, ...) {
  return(as_tibble(t(data_to_transpose), ...))
}

# debug_Helper <- function(latlon = "5930063.7517479582065.965598572") {
#   test <- tar_read(plz_group_1)
#   test <- filter(test, as.character(latlon_utm) == latlon)
#   data <<- test
# }



#this needs to cleaned up desperately 
# works pretty well tho
# somewhat slow, which_range can be moved to util

which_range = function(non_list_reason_vec = NA){
  
  # helps connect selling point to first preceding update
  # update chain upto and including selling point are connected into one listing
  
  # full NA vectors to return object of same length
  returner_end = returner_start = rep(NA,length(non_list_reason_vec))
  
  # id end of update chains i.e. when object was sold
  # update chains can length 0 if object is sold without being updated
  end_position = which(non_list_reason_vec == "Sold")
  
  # find subsequent end of update chain
  start_position = shift(end_position + 1, fill = 1, type = "shift")
  
  returner_start[end_position] = start_position
  returner_end[end_position] = end_position
  
  return(list(returner_start,returner_end))
}