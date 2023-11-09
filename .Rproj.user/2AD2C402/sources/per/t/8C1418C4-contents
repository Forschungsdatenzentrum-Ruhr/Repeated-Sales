classify_data <- function(geo_grouped_data = NA) {
  
  # group_mapping
  repeated_offerings <- geo_grouped_data %>%
    group_by(latlon_utm, balkon) %>%
    arrange(amonths) %>%
    group_modify(
      ~ removal(.x)
    )

  return(repeated_offerings)
}

