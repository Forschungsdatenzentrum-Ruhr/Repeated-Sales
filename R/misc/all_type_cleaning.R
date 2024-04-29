all_type_cleaning <- function(RED_classified, var_to_replace_missings) {
  #' @title Clean RED data for analysis same for all types
  #' 
  #' @description Clean RED data for analysis by replacing missing values with 0 and converting columns to factor.
  #' @param RED_classified data.table. RED data set with classification data.
  #' @param var_to_replace_missings character. Vector of variable names to be replaced.
  #' 
  #' @return data.table. RED data set with cleaned variables.
  #' @author Thorben Wiebe
  # ----------------------------------------------
  # Input validation
  input_check(RED_classified, "data.table")
  input_check(var_to_replace_missings, "character")
  #----------------------------------------------
  # this batch replaces all missings with zero and converts columns to factor
  RED_cleaned <- RED_classified[,
    (var_to_replace_missings) := lapply(.SD, function(x) {
      fifelse(x < 0, 0, x) |> as.factor()
    }),
    .SDcols = var_to_replace_missings
  ][,
    ## paired mutations
    # this only split for readability, could be combined with single mutations
    c("baujahr_cat") := lapply(.SD, function(x) {
      fcase(
        x <= 0, 0,
        x < 1900, 1,
        between(x, 1900, 1945), 2,
        between(x, 1946, 1959), 3,
        between(x, 1960, 1969), 4,
        between(x, 1970, 1979), 5,
        between(x, 1980, 1989), 6,
        between(x, 1990, 1999), 7,
        between(x, 2000, 2009), 8,
        x > 2010, 9
      ) |>
        factor(
          levels = 0:9,
          labels = c(
            "MISSING",
            "<1900",
            "1900-1945",
            "1946-1959",
            "1960-1969",
            "1970-1979",
            "1980-1989",
            "1990-1999",
            "2000-2009",
            ">2010"
          )
        )
    }),
    .SDcols = c("baujahr")
  ][
    ## single mutations
    , ":="(
      first_occupancy = fifelse(
        objektzustand == 1, 1, 0
      ) |> factor(
        0:1,
        c(
          "First occupancy",
          "Not first occupancy"
        )
      ),
      ausstattung = factor(
        ausstattung,
        0:4,
        c("MISSING", "Simple", "Normal", "Sophisticated", "Deluxe")
      )
    )
  ][!is.na(ausstattung) | !is.na(baujahr_cat)]

  #----------------------------------------------
  # Unit test
  tar_assert_true(
    all(
      c("baujahr_cat", "first_occupancy", "ausstattung") %in% names(RED_cleaned)),
      msg = "Not all essential variables are present in the data."
  )
  empty_check(RED_cleaned)
  #----------------------------------------------
  return(RED_cleaned)
}
