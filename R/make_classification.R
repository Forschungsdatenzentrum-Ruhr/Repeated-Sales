make_classification <- function(geo_grouped_data = NA) {

  # arrange data and prep for grouping
  # NOTE: use index instead ?
  setDT(geo_grouped_data, key = c("latlon_utm", "balkon", "amonths"))

  # extract end_date of data
  # NOTE: there should be a better place for this?
  data_end_date <- geo_grouped_data[, max(emonths)]

  
  pre_removal_obs <- geo_grouped_data[, .N]
  logger::log_info("Pre Removal Observations: ", pre_removal_obs)

  # classify parent-child relationships, adds sim_index and sim_distance
  geo_grouped_data_similarity <- geo_grouped_data[,
    #curly brackets causes both expressions to be evaluated but only last one is passed along
    {
      custom_progress_bar("Similarity", .GRP, .NGRP);
      similarity_classification(.SD)
      
    },
    by = c("latlon_utm", "balkon")
  ]

  # everything was classified
  tar_assert_true(pre_removal_obs == geo_grouped_data_similarity[, .N])

  # create panel structure based on parent-child relationship created above
  # connects listings based on months between occurences
  geo_grouped_data_connected <- geo_grouped_data_similarity[,
  {
    custom_progress_bar("Connected", .GRP, .NGRP);
    non_list_classification(.SD, data_end_date)
  },
    by = parent
  ]
  
  post_update_obs <- geo_grouped_data_connected[, .N]
  logger::log_info("Post Removal Observations: ", post_update_obs)


  return(geo_grouped_data_connected)
}
