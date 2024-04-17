read_RED <- function(RED_file_name) {
  #' @title Read RED data
  #'
  #' @description Read RED data from Stata file and mutate essential variables.
  #' @param RED_file_name character. Full file path of the RED data file.
  #'
  #' @return data.table. RED data set with essential variables.
  #' @author Thorben Wiebe
  #----------------------------------------------
  # Input validation
  input_check(RED_file_name, "character")
  #----------------------------------------------
  # read stata file and remove labels
  RED_full_data <- haven::read_dta(RED_file_name) |>
    haven::zap_labels() |>
    data.table::as.data.table()

  # mutations
  RED_full_data[
    ,
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
  # HK specific change
  if (str_detect(RED_file_name, "HK_")) {
    # reassign etage to anzahletage since etage is not used in HK
    RED_full_data[, etage := anzahletagen]
  }
  # set key for faster merging
  setkey(RED_full_data, counting_id)

  #----------------------------------------------
  # Unit test
  tar_assert_true(
    all(
      c("latlon_utm", "amonths", "emonths", "price_var", "counting_id") %in% names(RED_full_data),
      msg = "Not all essential variables are present in the data."
    )
  )
  empty_check(RED_full_data)
  #----------------------------------------------
  return(RED_full_data)
}
