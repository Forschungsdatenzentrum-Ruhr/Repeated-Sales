plot_split <- function(indicies, data_type) {
  #' @title Plot split indices
  #'
  #' @description Plot split indices
  #' @param indicies data.table. Split indices data set.
  #' @param data_type character. Type of data to be used. One of:
  #' *WK = Wohnungskauf/ Apartments for sale
  #' *WM = Wohnungsmiete/ Apartments for rent
  #' *HK = Hauskauf/ Houses for sale
  #'
  #' @return NULL
  #' @author Thorben Wiebe
  #----------------------------------------------
  # Input validation
  input_check(indicies, "data.table")
  input_check(data_type, "character")
  tar_assert_true(data_type %in% c("WK", "WM", "HK"), "data_type must be one of WK, WM, HK")
  #----------------------------------------------

  for (single_index_type in unique(indicies$index_type)) {
    # subset data
    single_index <- indicies[index_type == single_index_type]

    # default
    plot1 <- ggplot(single_index, aes(x = date_quarter, y = based_index, color = gid_names, group = gid_names)) +
      geom_line(aes(x = date_quarter, y = based_index, color = gid_names, group = gid_names)) +
      coord_cartesian(expand = FALSE) +
      theme_bw()
    ggsave(glue::glue("output/figures/{data_type}_{single_index_type}_gid_indicies.png"), plot1, width = 10, height = 10)

    # smmoth
    plot2 <- ggplot(single_index, aes(x = date_quarter, y = based_index, color = gid_names, group = gid_names)) +
      stat_smooth(aes(x = date_quarter, y = based_index, color = gid_names, group = gid_names), formula = y ~ s(x, bs = "cs"), method = "gam", se = F) +
      coord_cartesian(expand = FALSE) +
      theme_bw()
    ggsave(glue::glue("output/figures/{data_type}_{single_index_type}_smooth_gid_indicies.png"), plot2, width = 10, height = 10)
  }
  # ----------------------------------------------
  return(NULL)
}
