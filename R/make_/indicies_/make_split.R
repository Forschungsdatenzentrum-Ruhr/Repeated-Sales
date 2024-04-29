make_split = function(repeated_index, hybrid_index, hedonic_index){
  #' @title Make split
  #' 
  #' @description This function prepares the indices for the split.
  #' @param repeated_index data.table. Repeated index data.
  #' @param hybrid_index data.table. Hybrid index data.
  #' @param hedonic_index data.table. Hedonic index data.
  #' 
  #' @return data.table. Prepared indices for the split.
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

    # combine split index
    all_indices = list(
      "GRS" = GRS_index,
      "ARS" = ARS_index,
      "hybrid_index" = hybrid_index,
      "hedonic_index" = hedonic_index
    )
    # prepare split index
    indicies = rbindlist(
        # imap is just lapply with names (y = name of list element)
        purrr::imap(
            all_indices,
            function(x,y){prepare_split(x,y, c("date_quarter", "gid2019"))}  
    )
    )
    
    # rebase values to make them comparable
    base_values =  indicies[date_quarter == base_quarter, .(base_index = mean_index), by = c("gid2019","index_type")]
    
    # Unit test: check if all gid2019 are present in the base_values
    tar_assert_true(nrow(base_values) == uniqueN(indicies$gid2019)*length(all_indices), msg = "Not all indices have a base value")
    
    # calculate based index
    indicies = indicies[base_values, on = c("gid2019","index_type")][, based_index := (mean_index/base_index)*100]

    #----------------------------------------------
    # Unit test
    empty_check(indicies)
    #----------------------------------------------
    return(indicies)
}


