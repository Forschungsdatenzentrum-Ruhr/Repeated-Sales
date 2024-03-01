plot_combined = function(combined_indices, data_type){
    plot1 = ggplot(combined_indices) + 
    stat_smooth( aes(x = date_quarter, y = based_index, color = index_type, group = index_type), formula = y ~ s(x, bs = "cs"), method = "gam", se = F) +
    theme_bw()
    ggsave("output/figures/{data_type}_combined_indicies.png", plot1, width = 10, height = 10)

    return(NULL)
}
 