prepare_repeated <- function(RED_classified = NA, grouping_var = NA) {
  vars_needed <- c("rs_id", "emonths", "price_var", grouping_var)
  # tst_vars_needed = c(vars_needed, "latlon_utm","kid2015","gid2015","gid2019")

  rs_pairs_prep <- RED_classified["Sold", on = "non_list_reason", ..vars_needed]
  setkey(rs_pairs_prep, "rs_id")

  # reverse year to month conversion done during initial reading since subsequent functions require dates
  rs_pairs_prep <- rs_pairs_prep[
    , ":="(
      year = emonths %/% 12,
      month = emonths - ((emonths %/% 12) * 12)
    )
  ][
    # december is converted to an additional year, is this already a problem before this?
    # maybe use yearmon from zoo instead, shouldnt be a big change
    month == 0,
    ":="(
      month = 12,
      year = year - 1
    )
  ][, ":="(
    # sprintf is used to pad leading zeros for months while pasteing at the same time
    # %d means digits
    # %02d means digit with leading zeros until length 2
    date = sprintf(
      "%d-%02d-01",
      year,
      month
    ) |> as.Date(format = "%Y-%m-%d"),
    # drop year + month columns
    year = NULL,
    month = NULL,
    emonths = NULL
  )]

  # extract columns whose names are getting i. prefix during self-merge
  prev_cols <- c("price_var", "date")


  # self-merge data to construct required data structure
  # this is a called an update join
  self_merged_rs_pairs_prep <- rs_pairs_prep[
    rs_pairs_prep,
    on = c("rs_id==rs_id", "date>date"),
    # rename columns for clarity
    (glue::glue("prev_{prev_cols}")) := mget(glue::glue("i.{prev_cols}"))
  ]|> drop_na()
  
  self_merged_rs_pairs_prep = self_merged_rs_pairs_prep[,.SD[.N >= 2], by = "rs_id"]
  
  tar_assert_true(self_merged_rs_pairs_prep[prev_date > date, .N] == 0)

  return(self_merged_rs_pairs_prep)
}
