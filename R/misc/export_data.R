export_data <- function(classification, data_type) {
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
  final_path = glue::glue("{output_path}/RED_RSR_{data_type}_{RED_version}.fst")
  
  classification |>
      # write to file
     fst::write.fst(
      path = final_path
    )
  
  return(NULL)
}
