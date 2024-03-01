plot_split = function(indicies, data_type){

for(single_index_type in unique(indicies$index_type)){

        # subset data
        single_index = indicies[index_type == single_index_type]

        plot1 = ggplot(single_index, aes(x = date_quarter, y = based_index, color = gid2019, group = gid2019)) + 
        stat_smooth( aes(x = date_quarter, y = based_index, color = gid2019, group = gid2019), formula = y ~ s(x, bs = "cs"), method = "gam", se = F) +
        theme_bw()
        ggsave(glue::glue("output/figures/{data_type}_{single_index_type}_gid_indicies.png"), plot1, width = 10, height = 10)

    }

    return(NULL)

}