make_classification <- function(geo_grouped_data = NA) {
  #tar_load(classification_77a61db3)
  #geo_grouped_data = classification_77a61db3
  
  # arrange data and prep for grouping 
  # add balkon
  setkey(geo_grouped_data, latlon_utm,amonths)
  
  # add balkon to by
  geo_grouped_data[, removal(.SD), by = c("latlon_utm")]
  
  # # group_mapping
  # repeated_offerings <- geo_grouped_data %>%
  #   group_by(latlon_utm, balkon) %>%
  #   arrange(amonths) %>%
  #   group_modify(
  #     ~ removal(.x)
  #   )

  return(geo_grouped_data)
}

