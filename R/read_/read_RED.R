read_RED <- function(RED_file_name = NA) {
  #' @title WIP
  #'
  #' @description WIP
  #' @param WIP
  #' @param WIP
  #' @note
  #'
  #' @return WIP
  #' @author Thorben Wiebe
  #----------------------------------------------

  # read stata file and remove labels
  RED_all_columns <- haven::read_dta(RED_file_name) |>
    haven::zap_labels() |>
    data.table::setDT(key = c("blid", "ajahr", "amonat"))

  RED_all_columns = RED_all_columns[,
    ## mutations
    ":="(
      # combine coordinates
      latlon_utm = paste0(lat_utm, lon_utm),

      # transform years into months and add running years months
      amonths = ajahr * 12 + amonat,
      emonths = ejahr * 12 + emonat,

      # combine price and rent into one variable
      price_var = pmax(mietekalt, kaufpreis),

      # create unique id for entire data
      counting_id = 1:.N

    )
  ]
  
  return(RED_all_columns)
}
