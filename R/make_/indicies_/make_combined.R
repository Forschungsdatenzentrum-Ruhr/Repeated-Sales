make_combined = function(repeated_index, hybrid_index, hedonic_index){
  #' @title WIP
  #'
  #' @description WIP
  #' @param WIP
  #' @param WIP
  #' @note
  #'
  #' @return WIP
  #' @author Thorben Wiebe
  #----------------------------------------------

    # extract individual indicies
    repeated_index = split(repeated_index, by = "i_type", sorted = T, keep.by = F)
    GRS_index = repeated_index[["GRS"]]
    ARS_index = repeated_index[["ARS"]]
    
    all_indices = list(
      "GRS" = GRS_index,
      "ARS" = ARS_index,
      "hybrid_index" = hybrid_index,
      "hedonic_index" = hedonic_index
    )
    
    # prepare combined index
    indicies = rbindlist(

        purrr::imap(
            all_indices,
            function(x,y){prepare_combined(x,y, "date_quarter")}  
    )
    )

    # issue : 14612000 ARS
    
    # rebase values to make them comparable
    base_values =  indicies[date_quarter == base_quarter, .(base_index = mean_index), by = "index_type"]
    
    tar_assert_true(nrow(base_values) == length(all_indices))
    
    # calculate based index
    indicies = indicies[base_values, on = "index_type"][, based_index := (mean_index/base_index)*100]

    #----------------------------------------------
    return(indicies)
    
}


