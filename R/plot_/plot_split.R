plot_split = function(indicies, data_type){

for(single_index_type in unique(indicies$index_type)){

        # subset data
        single_index = indicies[index_type == single_index_type]

        # default
        plot1 = ggplot(single_index, aes(x = date_quarter, y = based_index, color = gid2019, group = gid2019)) + 
        geom_line(aes(x = date_quarter, y = based_index, color = gid2019, group = gid2019)) +
        coord_cartesian(expand = FALSE) + 
        theme_bw()
        ggsave(glue::glue("output/figures/{data_type}_{single_index_type}_gid_indicies.png"), plot1, width = 10, height = 10)
        
        # smmoth
        plot2 = ggplot(single_index, aes(x = date_quarter, y = based_index, color = gid2019, group = gid2019)) + 
          stat_smooth(aes(x = date_quarter, y = based_index, color = gid2019, group = gid2019), formula = y ~ s(x, bs = "cs"), method = "gam", se = F) +
          coord_cartesian(expand = FALSE) + 
          theme_bw()
        ggsave(glue::glue("output/figures/{data_type}_{single_index_type}_smooth_gid_indicies.png"), plot2, width = 10, height = 10)

    }

    return(NULL)

}