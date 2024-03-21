export_data <- function(classification = NA, data_version = NA, data_type = NA) {
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

  # paste path together
  final_path = glue::glue("{output_path}/classification_{data_type}_{data_version}.fst")
  
  #subset data to only contain sell events
  classification |>
      # write to file
     fst::write.fst(
      path = final_path
    )
  
  return(NULL)
}
