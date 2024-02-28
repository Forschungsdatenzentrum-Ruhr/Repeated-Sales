make_split = function(repeated_index, hybrid_index, hedonic_index){
#    tar_load(hedonic_index)
#    tar_load(hybrid_index)
#    tar_load(repeated_index) 
#    tar_load_globals()

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


    for(single_index_type in unique(indicies$index_type)){

        # subset data
        single_index = indicies[index_type == single_index_type]

        plot1 = ggplot(single_index, aes(x = date_quarter, y = based_index, color = gid2019, group = gid2019)) + 
        stat_smooth( aes(x = date_quarter, y = based_index, color = gid2019, group = gid2019), formula = y ~ s(x, bs = "cs"), method = "gam", se = F) +
        theme_bw()
        ggsave(glue::glue("output/figures/{single_index_type}_gid_indicies.png"), plot1, width = 10, height = 10)

    }

    





    return(indicies)
    
}


