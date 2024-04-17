function_template <- function(input1) {
  #' @title
  #'
  #' @description
  #' @param
  #'
  #' @return
  #'
  #' @author Thorben Wiebe
  #----------------------------------------------
  # Input validation
  tar_assert_true(
    inherits(input1, "data.table"),
    "input1 must be a data.table."
  )
  #----------------------------------------------
  out <- NULL
  #----------------------------------------------
  # Unit test
  tar_assert_true(
    all(dim(out) %in% c(0, NULL)),
    "Data is empty."
  )
  #----------------------------------------------
  return(out)
}
