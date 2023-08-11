export_data <- function(classification = NA, file_name = NA) {
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
  final_path = glue::glue("{output_path}/classification_{file_name}.fst")
  
  # subset data to only contain sell events
  classification["Sold", on = "non_list_reason"] |>
      # write to file
     fst::write.fst(
      path = final_path
    )
  
  return(final_path)
}
