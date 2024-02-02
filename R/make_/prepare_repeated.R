prepare_repeated <- function(RED_classified = NA, grouping_var = NA) {
  vars_needed <- c("rs_id", "emonths", "price_var", grouping_var)
  # tst_vars_needed = c(vars_needed, "latlon_utm","kid2015","gid2015","gid2019")

  rs_pairs_prep <- RED_classified["Sold", on = "non_list_reason", ..vars_needed]
  setkey(rs_pairs_prep, "rs_id")

  rs_pairs_prep <- make_date_quarter(rs_pairs_prep)

  # extract columns whose names are getting i. prefix during self-merge
  prev_cols <- c("price_var", "date_month", "date_quarter")


  # self-merge data to construct required data structure
  # this is a called an update join
  self_merged_rs_pairs_prep <- rs_pairs_prep[
    rs_pairs_prep,
    on = c("rs_id==rs_id", "date_month>date_month"),
    # rename columns for clarity
    (glue::glue("prev_{prev_cols}")) := mget(glue::glue("i.{prev_cols}"))
  ]|> drop_na()
  
  self_merged_rs_pairs_prep = self_merged_rs_pairs_prep[,.SD[.N >= 2], by = "rs_id"]
  
  tar_assert_true(self_merged_rs_pairs_prep[prev_date_month > date_month, .N] == 0)

  return(self_merged_rs_pairs_prep)
}
