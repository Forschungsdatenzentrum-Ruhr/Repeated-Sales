make_example_markdown_data <- function(geo_grouped_data) {
  #' @title Make example markdown data
  #' 
  #' @description Make example markdown data.
  #' @param geo_grouped_data data.table. Data set with geo grouped data.
  #' 
  #' @return data.table. Data set with example markdown data.
  #' @author Thorben Wiebe
  # ----------------------------------------------
  # Input validation
  input_check(geo_grouped_data, "data.table")
  #----------------------------------------------
  # tabulate coordinate counts in blid
  obs_coord <- table(geo_grouped_data[, latlon_utm])
  
  # extract first coordinate with 25 obs
  most_obs_coord <- first(names(obs_coord[obs_coord == 25]))
  
  # filter data for this coordinate and only consider with balcony, drop coordinate, blid and balkon
  out <- geo_grouped_data[
    latlon_utm == most_obs_coord & balkon == 1
  ][
    ,
    ":="(latlon_utm = NULL, blid = NULL, balkon = NULL)
  ]
  #----------------------------------------------
  # Unit test
  empty_check(out)
  #----------------------------------------------
  return(out)
}
