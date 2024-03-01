make_split = function(repeated_index, hybrid_index, hedonic_index){

    repeated_index = split(repeated_index, by = "i_type", sorted = T, keep.by = F)
    GRS_index = repeated_index[["GRS"]]
    ARS_index = repeated_index[["ARS"]]

    indicies = rbindlist(

        purrr::imap(
            list(
                "GRS" = GRS_index,
                "ARS" = ARS_index,
                "hybrid_index" = hybrid_index,
                "hedonic_index" = hedonic_index
                ),
            function(x,y){prepare_split(x,y)}  
    )
    )
    
    # rebase values to make them comparable
    base_quarter = "2007-02-01"
    base_values =  indicies[date_quarter == base_quarter, .(base_index = mean_index), by = c("gid2019","index_type")]
    indicies = indicies[base_values, on = c("gid2019","index_type")][, based_index := (mean_index/base_index)*100]

    return(indicies)
    
}


