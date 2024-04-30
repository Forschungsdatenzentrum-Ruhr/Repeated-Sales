make_combined = function(repeated_index, hybrid_index, hedonic_index){
  #' @title Make Combined Index
  #'
  #' @description Combine the repeated, hybrid and hedonic indices into one
  #' @param repeated_index data.table. Repeated index
  #' @param hybrid_index data.table. Hybrid index
  #' @param hedonic_index data.table. Hedonic index
  #'
  #' @return data.table. Combined index based in base_quarter (see _targets.R)
  #' @author Thorben Wiebe
  #----------------------------------------------
  # Input validation
  input_check(repeated_index, "data.table")
  input_check(hybrid_index, "data.table")
  input_check(hedonic_index, "data.table")
  #----------------------------------------------

    # extract individual indicies
    repeated_index = split(repeated_index, by = "i_type", sorted = T, keep.by = F)
    GRS_index = repeated_index[["GRS"]]
    ARS_index = repeated_index[["ARS"]]
    
    # combine combined index
    all_indices = list(
      "GRS" = GRS_index,
      "ARS" = ARS_index,
      "hybrid_index" = hybrid_index,
      "hedonic_index" = hedonic_index
    )
    
    # prepare combined index
    indicies = rbindlist(
        # imap is just lapply with names (y = name of list element)
        purrr::imap(
            all_indices,
            function(x,y){prepare_combined(x,y, "date_quarter")}  
    )
    )
    # rebase values to make them comparable
    base_values =  indicies[date_quarter == base_quarter, .(base_index = mean_index), by = "index_type"]
    
    #tar_assert_true(nrow(base_values) == length(all_indices), msg = "Not all indices have a base value")
    
    # calculate based index
    indicies = indicies[base_values, on = "index_type"][, based_index := (mean_index/base_index)*100]
    # cut off at base_quarter (not sure if i want this)
    indicies = indicies[date_quarter >= base_quarter]
    #----------------------------------------------
    # Unit test
    empty_check(indicies)
    #----------------------------------------------
    return(indicies)
    
}


