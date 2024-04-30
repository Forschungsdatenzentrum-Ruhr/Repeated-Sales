prepare_repeated <- function(RED_subset_classified, grouping_var) {
  #' @title Prepare repeated
  #' 
  #' @description This function prepares the repeated sales index.
  #' @param RED_subset_classified data.table. RED classified data.
  #' @param grouping_var character. Grouping variable.
  #' 
  #' @return data.table. Prepared repeated sales index.
  #' @author Thorben Wiebe
  # ----------------------------------------------
  # Input validation
  input_check(RED_subset_classified, "data.table")
  input_check(grouping_var, "character")
  # ----------------------------------------------
  
  # declare which variables are used during preparation
  vars_needed <- c("rs_id", "emonths", "price_var", grouping_var)

  # subset data to only include sold properties and the required variables
  rs_pairs_prep <- RED_subset_classified["Sold", on = "non_list_reason", ..vars_needed]
  setkey(rs_pairs_prep, "rs_id")

  # add date_quarter column
  rs_pairs_prep <- make_date_quarter(rs_pairs_prep)

  # extract columns whose names are getting i. prefix during self-merge
  prev_cols <- c("price_var", "date_month", "date_quarter")

  # self-merge data to construct required data structure
  # this is a called an update join
  prepared_repeated_prep <- rs_pairs_prep[
    rs_pairs_prep,
    on = c("rs_id==rs_id", "date_month>date_month"),
    # rename columns for clarity
    (glue::glue("prev_{prev_cols}")) := mget(glue::glue("i.{prev_cols}"))
  ]|> drop_na()
  
  # filter out outlines and single repeats
  prepared_repeated_prep = prepared_repeated_prep[
    # heavy individual outliers
    !rs_id %in% c(4144879)
    ][
    ,.SD[.N >= 2], by = "rs_id"]
  
  # ----------------------------------------------
  # Unit test
  empty_check(prepared_repeated_prep)
  # check if date month is in order such that no sale is before the previous sale
  tar_assert_true(prepared_repeated_prep[prev_date_month > date_month, .N] == 0, msg = "Date month is not in order")
  # ----------------------------------------------
  return(prepared_repeated_prep)
}
