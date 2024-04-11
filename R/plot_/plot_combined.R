plot_combined = function(combined_indices, data_type){
    
    # default
    plot1 = ggplot(combined_indices) +
    geom_line( aes(x = date_quarter, y = based_index, color = index_type, group = index_type)) +
    coord_cartesian(expand = FALSE) +
    theme_bw()
    ggsave(glue::glue("output/figures/{data_type}_combined_indicies.png"), plot1, width = 10, height = 10)
    
    # smooth
    plot2 = ggplot(combined_indices) + 
      stat_smooth( aes(x = date_quarter, y = based_index, color = index_type, group = index_type), formula = y ~ s(x, bs = "cs"), method = "gam", se = F) +
      coord_cartesian(expand = FALSE) +
      theme_bw()
    ggsave(glue::glue("output/figures/{data_type}_smooth_combined_indicies.png"), plot2, width = 10, height = 10)

    return(NULL)
}
 