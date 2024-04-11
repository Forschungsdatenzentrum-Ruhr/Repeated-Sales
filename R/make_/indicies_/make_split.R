make_split = function(repeated_index, hybrid_index, hedonic_index){

    repeated_index = split(repeated_index, by = "i_type", sorted = T, keep.by = F)
    GRS_index = repeated_index[["GRS"]]
    ARS_index = repeated_index[["ARS"]]

    all_indices = list(
      "GRS" = GRS_index,
      "ARS" = ARS_index,
      "hybrid_index" = hybrid_index,
      "hedonic_index" = hedonic_index
    )
    
    indicies = rbindlist(

        purrr::imap(
            all_indices,
            function(x,y){prepare_split(x,y, c("date_quarter", "gid2019"))}  
    )
    )
    
    # rebase values to make them comparable
    base_values =  indicies[date_quarter == base_quarter, .(base_index = mean_index), by = c("gid2019","index_type")]
    
    tar_assert_true(nrow(base_values) == uniqueN(indicies$gid2019)*length(all_indices))
    
    indicies = indicies[base_values, on = c("gid2019","index_type")][, based_index := (mean_index/base_index)*100]

    return(indicies)
    
}


