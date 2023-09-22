make_repeated <- function(classification = NA) {
  library(rsmatrix)
  # vars_needed = c("counting_id","parent","emonths","kaufpreis","latlon_utm")
  vars_needed <- c("counting_id", "parent", "emonths", "price_var")

  rs_pairs <- classification[non_list_reason == "Sold", ..vars_needed]
  setkey(rs_pairs, "parent")


  # reverse year to month conversion done during initial reading since subsequent functions require dates
  rs_pairs[
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
  ]

  rs_pairs[, ":="(
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
  prev_cols <- setdiff(intersect(names(rs_pairs), names(rs_pairs)), "parent")

  # self-merge data to construct required data structure
  self_merged_rs_pairs <- rs_pairs[rs_pairs, on = c("parent==parent"), nomatch = 0]
  # rename columns for clarity
  setnames(self_merged_rs_pairs, glue::glue("i.{prev_cols}"), glue::glue("prev_{prev_cols}"))

  # drop merges made on exact same listing
  # taken from rsmatrix vignette
  matrices <- with(
    self_merged_rs_pairs[
      # listings merged on themselves
      counting_id != prev_counting_id &
        # opposite shouldnt be occuring, have to check
        date > prev_date &
        # price = 0 throws error
        price_var != 0 &
        prev_price_var != 0
    ],
    rs_matrix(date, prev_date, price_var, prev_price_var, sparse = T)
  )

  # GRS

  Z <- matrices("Z")
  y <- matrices("y")
  y
  grs <- exp(solve(crossprod(Z), crossprod(Z, y)))
}
