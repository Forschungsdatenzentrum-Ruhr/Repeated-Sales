remerge_RED <- function(classification, RED_full_data) {
  #' @title Merge RED data with classification
  #'
  #' @description Merge RED data with classification data.
  #'
  #' @param classification data.table. Classification data.
  #' @param RED_full_data data.table. RED data set with essential variables.
  #'
  #' @return data.table. RED data set with classification data.
  #'
  #' @author Thorben Wiebe
  #----------------------------------------------
  # Input validation
  input_check(classification, "data.table")
  input_check(RED_full_data, "data.table")
  #----------------------------------------------

  # find all columns that are present in both, classification columns take precedence
  # disregarding counting_id, since this is used for the merge
  names_diff <- setdiff(intersect(names(classification), names(RED_full_data)), "counting_id")

  # remove columns from RED and merge classifcation
  RED_classified <- RED_full_data[, !..names_diff][classification, on = "counting_id"]

  # subset to 15 biggest citiess
  big_fifteen <- data.table(
    gid2019 =c(
    "11000000", # Berlin
    "02000000", # Hamburg
    "09162000", # München
    "05315000", # Köln
    "06412000", # Frankfurt
    "08111000", # Stuttgart
    "05111000", # Düsseldorf
    "14713000", # Leipzig
    "05913000", # Dortmund
    "05113000", # Essen
    "04011000", # Bremen
    "14612000", # Dresden
    "03241001", # Hannover
    "09564000", # Nürnberg
    "05112000" # Duisburg
  ) |> as.numeric()
  )

  RED_classified <- RED_classified[big_fifteen, on = "gid2019"]
  
  ## do some outlier removal for all types and indicies
  # force only whole numbers 
  RED_classified = RED_classified[price_var >= 0, RED_classified := round(price_var,0)]
  
  # filter top bottom percent of price variables to catch incredible outlines
  a = 1
  # prepare percentiles for price
  upper_percentile_price_var <- quantile(RED_classified[price_var >= 0, price_var], 1 - (a  / 100))
  lower_percentile_price_var <- quantile(RED_classified[price_var >= 0, price_var], (a  / 100))
  
  # prepare percentiles for wohnflaeche
  upper_percentile_wohnflaeche <- quantile(RED_classified[wohnflaeche >= 0, wohnflaeche], 1 - (a / 100))
  lower_percentile_wohnflaeche <- quantile(RED_classified[wohnflaeche >= 0, wohnflaeche], (a / 100))
  
  RED_classified = RED_classified[
    price_var %between% c(lower_percentile_price_var, upper_percentile_price_var) & 
    wohnflaeche %between% c(lower_percentile_wohnflaeche, upper_percentile_wohnflaeche)
  ]

  #----------------------------------------------
  # Unit test
  tar_assert_true(
    all(
      c("counting_id", names_diff) %in% names(RED_classified)),
      msg = "Not all essential variables are present in the data."
  )
  empty_check(RED_classified)
  #----------------------------------------------
  return(RED_classified)
}
