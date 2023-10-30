make_repeated <- function(RED_classified = NA) {
  library(rsmatrix)

  vars_needed <- c("counting_id", "rs_id", "emonths", "kaufpreis", "kid2019")
  # tst_vars_needed = c(vars_needed, "latlon_utm","kid2015","gid2015","gid2019")

  rs_pairs <- RED_classified["Sold", on = "non_list_reason", ..vars_needed]
  setkey(rs_pairs, "rs_id", "counting_id")

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
  #prev_cols <- setdiff(intersect(names(rs_pairs), names(rs_pairs)),"rs_id")
  prev_cols <- c("kaufpreis","date","counting_id")
  
  # self-merge data to construct required data structure
  # this is a called an update join
  self_merged_rs_pairs <- rs_pairs[
    rs_pairs, 
    on = c("rs_id==rs_id","date>date"),
    # rename columns for clarity
    (glue::glue("prev_{prev_cols}")) := mget(glue::glue("i.{prev_cols}"))
    ]

  tar_assert_true(self_merged_rs_pairs[prev_date > date, .N] == 0)
  
  # taken from rsmatrix vignette
  matrices <- with(
    # na.omit here since update joins ignore nomatch = 0 argument
    na.omit(self_merged_rs_pairs),
    rs_matrix(
      t2 = date,
      t1 = prev_date,
      p2 = kaufpreis,
      p1 = prev_kaufpreis,
      f = kid2019, # grouping variable
      sparse = T
    )
  )

  View(GRS)
  # Prep --------------------------------------------------------------------

  Z <- matrices("Z")
  y <- matrices("y")
  X <- matrices("X")
  Y <- matrices("Y")

  # GRS ---------------------------------------------------------------------
  # index via Bailey(1963)
  grs_b <- qr.coef(qr(Z), y)

  GRS <- exp(grs_b) * 100
  # GRS_vcov <- rs_var(y - Z %*% b, Z) |>
  #   diag() |>
  #   sqrt()

  # ARS ---------------------------------------------------------------------

  # index via Shiller (1991)
  ars_b <- qr.coef(
    qr(
      Z %*% t(X),
      Z %*% t(Y)
    )
  )
  ARS <- 100 / ars_b
  # vcov <- rs_var(Y - X %*% ars_b, Z, X) |>
  #   diag() |>
  #   sqrt()
  # ARS_vcov <- vcov * ARS^2
  
}
