#' Helper and Debug Functions, WIP-Section
#' 
#' @description
#' Universal functions used during different steps of pipeline aswell as WIP-
#' Section and debugging
#'

output_path_json = function(output_path = NA){
  if(!dir.exists(output_path)){
    dir.create(output_path)
  }
  
  write_json(
    exportJSON, 
    paste0(output_path,"/","settings_used.json")
  )
  
}



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

custom_progress_bar = function(classification_type = NA, .GRP = NA,.GRPN = NA, mod = 1000){
  
  # the .envir argument causes the progress_bar used to be the global one
  if(.GRP == 1){
    
    start_time <<- Sys.time()
    # Initialization of progress bar and some stats
    cli::cli_alert_info(glue::glue("Starting {classification_type} at {start_time}" ))
    cli::cli_alert_info(glue::glue("Total groups: {.GRPN}"))
    cli::cli_progress_bar("  Classifying...", total = (.GRPN/mod), .envir = parent.frame(n = sys.nframe()))
  
  } else if(.GRP == .GRPN){
    
    # Finish and cleanup
    cli::cli_alert_success(glue::glue("Finished {classification_type} after {Sys.time() - start_time}"))
    cli::cli_progress_done(.envir = parent.frame(n = sys.nframe()))
    
  } else if(!(.GRP %% mod)){
    
    # Update everytime mod times x is hit
    cli::cli_progress_update(.envir = parent.frame(n = sys.nframe()))
  
  }
    
  return(NULL)
  
  
}
