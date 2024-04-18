make_date_quarter <- function(input_data) {
  #' @title Make date quarter
  #' 
  #' @description Make date quarter from emonths.
  #' @param input_data data.table. Data set with emonths.
  #' 
  #' @return data.table. Data set with date_quarter.
  #' @author Thorben Wiebe
  # ----------------------------------------------
  # Input validation
  input_check(input_data, "data.table")
  #----------------------------------------------
  # reverse year to month conversion done during initial reading since subsequent functions require dates
  out <- copy(input_data)[
    ,
    ":="(
      year = emonths %/% 12,
      month = emonths - ((emonths %/% 12) * 12)
    )
  ][
    # december is converted to an additional year
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
    date_month = sprintf(
      "%d-%02d-01",
      year,
      month
    ) |> as.Date(format = "%Y-%m-%d"))][
      ,
      ":="(
        date_quarter = sprintf(
          "%d-%02d-01",
          year,
          quarter(date_month)
        ) |> as.Date(format = "%Y-%m-%d"),
        year = NULL,
        month = NULL,
        emonths = NULL
      )
    ]
  #----------------------------------------------
  # Unit test
  tar_assert_true("date_quarter" %in% names(out), "date_quarter column not found.")
  empty_check(out)
  #----------------------------------------------
  return(out)
}
