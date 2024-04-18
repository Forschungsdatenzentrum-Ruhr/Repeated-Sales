make_classification <- function(geo_grouped_data) {
  #' @title Make classification of listings on corrdinate and by balcony
  #' 
  #' @description This function classifies listings based on their similarity and creates a parent-child relationship
  #' @param geo_grouped_data data.table. Data set with grouped data
  #' 
  #' @return data.table. Data set with classified listings
  #' @author Thorben Wiebe
  # ------------------------------------------------
  # Input validation
  input_check(geo_grouped_data, "data.table")
  # ------------------------------------------------

  # arrange/key data and prep for grouping and merging
  geo_grouped_data = data.table::data.table(geo_grouped_data, key = c("latlon_utm", "balkon", "counting_id"))

  # extract end_date of data
  data_end_date <- geo_grouped_data[, max(emonths)]

  # classify parent-child relationships, adds sim_index and sim_distance
  geo_grouped_data_similarity <- geo_grouped_data[,
    {
      # curly brackets causes both expressions to be evaluated but only last one is passed along
      custom_progress_bar("Similarity", .GRP, .NGRP);
      similarity_classification(.SD, unique(latlon_utm))
      
    },
    by = c("latlon_utm", "balkon")
  ]

  # Unit test: check if all listings are classified
  tar_assert_true(
    geo_grouped_data[,.N] == geo_grouped_data_similarity[, .N], 
    msg = glue::glue("Not all listings are classified: {head(geo_grouped_data[['latlon_utm']], 10)}")
    )

  # create panel structure based on parent-child relationship created above
  # connects listings based on months between occurrences
  geo_grouped_data_connected <- geo_grouped_data_similarity[,
  {
    # curly brackets causes both expressions to be evaluated but only last one is passed along
    custom_progress_bar("Connected", .GRP, .NGRP);
    non_list_classification(.SD, data_end_date)
  },
    by = parent
  ]
  # Unit test: check if all parents and ids are unique
  unique_parents = unique(geo_grouped_data_connected[,parent])
  unique_ids = unique(geo_grouped_data_connected[,counting_id])
  
  tar_assert_true(all(unique_parents %in% unique_ids), msg = glue::glue("Unequal parents and ids: {unique(geo_grouped_data_connected$latlon_utm)}"))
  # ------------------------------------------------
  # Unit test
  empty_check(geo_grouped_data_connected)
  # ------------------------------------------------
  return(geo_grouped_data_connected)
}
