#' WIP

#' @description
#' WIP
#'
#' @return
#' WIP
#'
#' @param .data A data frame or similar
#' @param t_offset time offset values
#'
#'
.data = data
removal <- function(.data, t_offset = time_offset) {
  # empty object to store repeated offerings
  repeated_offerings <- c()
  
  # remove dup rows
  .data %<>% distinct()
  
  ## iteratively and recursively consider each listing as potential parent (pp)
  while (nrow(.data) > 0) {
    candidates <- .data %>%
      
      # gen time difference relative to ...
      mutate(
        
        # last date in data
        td_to_end = (as.numeric(data_end_date) - as.numeric(emonths)),

        # leading offering
        td_to_lead = (lead(as.numeric(amonths)) - as.numeric(emonths)),

        # replace last td_to_lead with td_to_end
        # the last offering by definition can have no lagged offering
        # use end date to data instead
        td_to_lead = replace_na(td_to_lead, td_to_end[is.na(td_to_lead)])
      )
    # extract counting id of initial pp
    # to be removed later to guarantee avoidance of infinite looping
    inital_id <- candidates$counting_id[1]
    
    # drop all offerings that were listed less than six months after directly
    # lagged offering. we consider these to be updates rather than a full
    # buy-sell circle. it is possible at this point for the initial pp
    # to be dropped as well;  this is intended.
    # repeated/similarity considerations are only made afterwards and use first
    # non-update offering (as in the first offering actually sold)
    candidates %<>% filter(td_to_lead >= t_offset)
    
    ##
    if (nrow(candidates) > 0) {
      
      # calculate repeated id and similarity measure relative to pp
      ids <- candidates %>% summarise(repeat_similiarity(.))
      
      # attach repeated id and similarity to candidates
      repeats <- cbind(candidates, ids) %>%
        
        # drop non-repeated offerings
        filter(repeated_id != -1) %>%
        
        # assign pp counting id to all candidates as parent id
        # this implies a transition from candiate to actual repeat
        mutate(
          obj_parent = counting_id[1]
        )
      
      # remove inital pp and considered pp from .data
      # this is fine to always happen
      ids_to_remove <- c(repeats$counting_id[1], inital_id)
      
      .data %<>% filter(!counting_id %in% ids_to_remove)
      
      
      if (!is.null(repeated_offerings)){
        inter = intersect(repeated_offerings$counting_id, repeats$counting_id)
        print(inter)
         if(length(inter) > 0){
           print(inter)
         }
      }
      
      repeated_offerings <- repeats %>%
        bind_rows(repeated_offerings)
      
      
      
      #Stopped here
      # To DO
      # 1 Iteration: Add all repeats to repeated_offerings
      # Following I: 
      # i) Check for overlaps in counting id 
      # iia) compare
      
      
      ## resembling-overlaps
      # iib) compare similiarity of resembling-overlaps
      # iiba) old better match: do not add new
      # iibb) new better match: remove old, add new
      
      


      
    } else {
      .data %<>% filter(!counting_id %in% inital_id)
    }
    
  }
  return(repeated_offerings)
}
