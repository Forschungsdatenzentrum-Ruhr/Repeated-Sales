make_combined = function(repeated_index, hybrid_index, hedonic_index){
    # tar_load(hedonic_index)
    # tar_load(hybrid_index)
    # tar_load(repeated_index) 

    repeated_index = split(repeated_index, by = "i_type", sorted = T, keep.by = F)
    GRS_index = repeated_index[["GRS"]]
    ARS_index = repeated_index[["ARS"]]


    indicies = rbindlist(

        purrr::imap(
            list(
                "GRS" = GRS_index,
                #"ARS" = ARS_index,
                "hybrid_index" = hybrid_index,
                "hedonic_index" = hedonic_index
                ),
            function(x,y){prepare_combined(x,y)}  
    )
    )
    # rebase values to make them comparable
    base_quarter = "2007-02-01"
    base_values =  indicies[date_quarter == base_quarter, .(base_index = mean_index), by = c("type")]
    indicies = indicies[base_values, on = "type"][, based_index := (mean_index/base_index)*100]

    plot1 = ggplot(indicies, aes(x = date_quarter, y = based_index, color = type, group = type)) + 
    geom_line() + 
    theme_bw()
    ggsave("output/figures/combined_indicies.png", plot1, width = 10, height = 10)

indicies = rbindlist(

        purrr::imap(
            list(
                "GRS" = GRS_index
                #"ARS" = ARS_index,
                ),
            function(x,y){prepare_combined(x,y,c("date_quarter","gid2019"))}  
    )
    )
    # rebase values to make them comparable
    base_quarter = "2007-02-01"
    base_values =  indicies[date_quarter == base_quarter, .(base_index = mean_index), by = c("gid2019")]
    indicies = indicies[base_values, on = "gid2019"][, based_index := (mean_index/base_index)*100]

    plot1 = ggplot(indicies, aes(x = date_quarter, y = based_index, color = gid2019, group = gid2019)) + 
    geom_line() + 
    theme_bw()
    ggsave("output/figures/combined_indicies.png", plot1, width = 10, height = 10)





    return(indicies)
    
}

prepare_combined = function(single_index,single_index_name, grouping = c("date_quarter")){
    if (!("date_quarter" %in% names(single_index))){
        single_index = make_date_quarter(single_index)
    } else {
        # repeated index calculation combines the date_quarter and gid2019 columns
        # resplit them here
        
        gid_date = single_index[["date_quarter"]] |> tstrsplit("\\.") 
        single_index = single_index[, ":="(
            gid2019 = gid_date[[1]],
            date_quarter = gid_date[[2]] |> as.Date("%Y-%m-%d"),
            index = as.numeric(index)
        )]
    }
    # "gid2019"
    single_index = single_index[, .(mean_index = mean(index, na.rm = T)), by = grouping][, type := single_index_name]

    return(single_index)
}
