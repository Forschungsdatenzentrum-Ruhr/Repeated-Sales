make_sensitivity <- function(geo_grouped_data = NA, resembling_offset = NA, exact_offset = NA) {
  # NOTE: very similar to make_classification, 
  # some slight adjustments to account for repeated classification of same fs
  
  
  # overwrite global offsets
  wohnflaeche_r_o <<- resembling_offset
  wohnflaeche_e_o <<- exact_offset
  
  # arrange data and prep for grouping
  setDT(geo_grouped_data, key = c("latlon_utm", "balkon", "amonths"))

  # extract end_date of data
  data_end_date <- geo_grouped_data[, max(emonths)]
  
  # classify parent-child relationships, adds sim_index and sim_distance
  geo_grouped_data_similarity <- geo_grouped_data[,
    # curly brackets causes both expressions to be evaluated but only last one is passed along
    {
      custom_progress_bar("Similarity", .GRP, .NGRP)
      similarity_classification(.SD)
    },
    by = c("latlon_utm", "balkon")
  ]
  
  # create panel structure based on parent-child relationship created above
  # connects listings based on months between occurences
  geo_grouped_data_connected <- geo_grouped_data_similarity[,
    {
      custom_progress_bar("Connected", .GRP, .NGRP)
      non_list_classification(.SD, data_end_date)
    },
    by = parent
  ]
  # add parameters used to data for plotting
  geo_grouped_data_connected[,
    ":="(
      wohnflaeche_r_o = wohnflaeche_r_o,
      wohnflaeche_e_o = wohnflaeche_e_o
      )
  ]


  return(geo_grouped_data_connected)
}
