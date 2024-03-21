make_classification <- function(geo_grouped_data = NA) {

  # it might help quite a bit to move this into a markdown file with a
  # synthetic example of how every step works. might be useful to structure
  # paper as wells

  # arrange data and prep for grouping
  geo_grouped_data = data.table::data.table(geo_grouped_data, key = c("latlon_utm", "balkon", "counting_id"))

  # extract end_date of data
  # NOTE: there should be a better place for this?
  data_end_date <- geo_grouped_data[, max(emonths)]

  # classify parent-child relationships, adds sim_index and sim_distance
  geo_grouped_data_similarity <- geo_grouped_data[,
    #curly brackets causes both expressions to be evaluated but only last one is passed along
    {
      custom_progress_bar("Similarity", .GRP, .NGRP);
      similarity_classification(.SD, unique(latlon_utm))
      
    },
    by = c("latlon_utm", "balkon")
  ]

  # check if everything was classified
  tar_assert_true(geo_grouped_data[,.N] == geo_grouped_data_similarity[, .N])

  # create panel structure based on parent-child relationship created above
  # connects listings based on months between occurrences
  
  # isnt it possible to drop the parent itself here?
  # might have to move the parent along to the first sell event aswell
  geo_grouped_data_connected <- geo_grouped_data_similarity[,
  {
    custom_progress_bar("Connected", .GRP, .NGRP);
    non_list_classification(.SD, data_end_date)
  },
    by = parent
  ]
  
  unique_parents = unique(geo_grouped_data_connected[,parent])
  unique_ids = unique(geo_grouped_data_connected[,counting_id])
  
  tar_assert_true(all(unique_parents %in% unique_ids), msg = glue::glue("{unique(geo_grouped_data_connected$latlon_utm)}"))
  
  
  # unit-test
  # this should basically never trigger since the unit-test
  # in non_list_classification() should catch errors beforehand. 
  # leaving it as backup for now 
  # check_nonsensical_listings(geo_grouped_data_connected,"geo_grouped_data_connected")

  return(geo_grouped_data_connected)
}
