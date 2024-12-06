plot_combined <- function(combined_indices, data_type) {
  #' @title Plot combined indices
  #'
  #' @description Plot combined indices
  #' @param combined_indices data.table. Combined indices data set.
  #' @param data_type character. Type of data to be used. One of:
  #' *WK = Wohnungskauf/ Apartments for sale
  #' *WM = Wohnungsmiete/ Apartments for rent
  #' *HK = Hauskauf/ Houses for sale
  #'
  #' @return NULL
  #' @author Thorben Wiebe
  # ----------------------------------------------
  # Input validation
  input_check(combined_indices, "data.table")
  input_check(data_type, "character")
  tar_assert_true(data_type %in% c("WK", "WM", "HK"), "data_type must be one of WK, WM, HK")
  #----------------------------------------------
  # prettify the types
  combined_indices[, index_type := fcase(
    index_type == "ARS", "ARS",
    index_type == "GRS", "GRS",
    index_type == "hybrid_index", "Hybrid",
    index_type == "hedonic_index", "Hedonic"
  )]
  #----------------------------------------------
  # write total points changes to file
  start_values = combined_indices[date_quarter == min(date_quarter), .(index_type, based_index)]
  end_values = combined_indices[date_quarter == max(date_quarter), .(index_type, based_index)]
  write_total_point_change(start_values, end_values, data_type, split_bool = FALSE)
  #----------------------------------------------

  # default
  plot1 <- ggplot(combined_indices) +
    geom_line(aes(x = date_quarter, y = based_index, color = index_type, group = index_type)) +
    ylab("Index Value") + 
    own_theme
  ggsave(glue::glue("output/{data_type}/{data_type}_combined_indicies.png"), plot1, dpi = 300)

  # smooth
  plot2 <- ggplot(combined_indices) +
    stat_smooth(aes(x = date_quarter, y = based_index, color = index_type, group = index_type), formula = y ~ s(x, bs = "cs"), method = "gam", se = F) +
    ylab("Index Value") + 
    own_theme
  ggsave(glue::glue("output/{data_type}/{data_type}_smooth_combined_indicies.png"), plot2, dpi = 300)

  #----------------------------------------------
  return(NULL)
}
