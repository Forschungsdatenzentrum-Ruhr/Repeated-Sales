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
  # prettify the types
  indicies[, index_type := fcase(
    index_type == "ARS", "ARS",
    index_type == "GRS", "GRS",
    index_type == "hybrid_index", "Hybrid",
    index_type == "hedonic_index", "Hedonic"
  )]
  # replace ids with names for plotting
  big_fifteen <- data.table(
    gid2019 =c(
      "11000000", # Berlin
      "02000000", # Hamburg
      "09162000", # München
      "05315000", # Köln
      "06412000", # Frankfurt
      "08111000", # Stuttgart
      "05111000", # Düsseldorf
      "14713000", # Leipzig
      "05913000", # Dortmund
      "05113000", # Essen
      "04011000", # Bremen
      "14612000", # Dresden
      "03241001", # Hannover
      "09564000", # Nürnberg
      "05112000" # Duisburg
    ) |> as.numeric(),
    gid_names = c(
      "Berlin",
      "Hamburg",
      "Munich",
      "Colonge",
      "Frankfurt",
      "Stuttgart",
      "Dusseldorf",
      "Leipzig",
      "Dortmund",
      "Essen",
      "Bremen",
      "Dresden",
      "Hannover",
      "Nurnberg",
      "Duisburg"
    ) |> as.factor()
  )
  indicies[, gid2019 := as.numeric(gid2019)]
  indicies = indicies[big_fifteen, on = "gid2019"]

  for (single_index_type in unique(indicies$index_type)) {
    # subset data
    single_index <- indicies[index_type == single_index_type]

    # default
    plot1 <- ggplot(single_index, aes(x = date_quarter, y = based_index, color = gid_names, group = gid_names)) +
      geom_line(aes(x = date_quarter, y = based_index, color = gid_names, group = gid_names)) +
      coord_cartesian(expand = FALSE) +
      ylab("Index Value") + 
      own_theme
    ggsave(glue::glue("output/{data_type}/{data_type}_{single_index_type}_gid_indicies.png"), plot1, dpi = 300)

    # smooth
    plot2 <- ggplot(single_index, aes(x = date_quarter, y = based_index, color = gid_names, group = gid_names)) +
      stat_smooth(aes(x = date_quarter, y = based_index, color = gid_names, group = gid_names), formula = y ~ s(x, bs = "cs"), method = "gam", se = F) +
      ylab("Index Value") + 
      own_theme
    ggsave(glue::glue("output/{data_type}/{data_type}_{single_index_type}_smooth_gid_indicies.png"), plot2, dpi = 300)
    # NOTE: Im pretty sure the values for the municipalites get swapped somewhere in the hybrid code -> i.e. Bremen becomes 
    # Berlin and vice versa - their indicies inversely match those in hedonic. No idea where/how though
    
  }
  #subset
  subset_gid2019 = big_fifteen[c("Berlin","Munich","Frankfurt"), on = "gid_names"]
  subset_indicies = indicies[subset_gid2019, on = "gid2019"][index_type %in% c("Hedonic","GRS","Hybrid")]

  # ----------------------------------------------
  # write total points changes to file
  start_values = subset_indicies[date_quarter == min(date_quarter), .(index_type, based_index, gid_names)]
  end_values = subset_indicies[date_quarter == max(date_quarter), .(index_type, based_index, gid_names)]
  write_total_point_change(start_values, end_values, data_type)
  # ----------------------------------------------
  
  plot3 = ggplot(subset_indicies, aes(x = date_quarter, y = based_index, color = gid_names, linetype = index_type)) +
    stat_smooth(aes(x = date_quarter, y = based_index, color = gid_names, linetype = index_type), formula = y ~ s(x, bs = "cs"), method = "gam", se = F) +
    ylab("Index Value") +
    own_theme
  ggsave(glue::glue("output/{data_type}/{data_type}_subset_indicies.png"), plot3, dpi = 300)
  # ----------------------------------------------
  return(NULL)
}
