make_classification <- function(geo_grouped_data = NA) {
  # there need to be some serious renaming in all of these functions

  # arrange data and prep for grouping
  # use index instead ?
  setDT(geo_grouped_data, key = c("latlon_utm", "balkon", "amonths"))

  # extract end_date of data
  # there should be a better place for this?
  data_end_date <- geo_grouped_data[, max(emonths)]


  pre_removal_obs <- geo_grouped_data[, .N]
  logger::log_info("Pre Removal Observations: ", pre_removal_obs)

  geo_grouped_data_similarity <- geo_grouped_data[,
    {
      similarity_classification(.SD)
      custom_progress_bar(.GRP, .NGRP)
    },
    by = c("latlon_utm", "balkon")
  ]

  # everything was classified
  tar_assert_true(pre_removal_obs == geo_grouped_data_similarity[, .N])

  # .SDcols = c("amonths","emonths")
  geo_grouped_data_connected <- geo_grouped_data_similarity[,
    {
      non_list_classification(.SD, data_end_date)
      custom_progress_bar(.GRP, .NGRP)
    },
    by = parent
  ]
  # [!"Update", on = "non_list_reason"]

  post_update_obs <- geo_grouped_data_connected[, .N]

  logger::log_info("Post Removal Observations: ", post_update_obs)


  return(geo_grouped_data_connected)
}
