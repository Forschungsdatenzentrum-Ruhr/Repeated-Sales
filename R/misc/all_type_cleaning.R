all_type_cleaning <- function(RED_classified, var_to_replace_missings) {
  # this batch replaces all missings with zero and converts columns to factor
  RED_cleaned <- RED_classified[,
    (var_to_replace_missings) := lapply(.SD, function(x) {
      fifelse(x < 0, 0, x) |> as.factor()
    }),
    .SDcols = var_to_replace_missings
  ][,
    # paired mutations --------------------------------------------------------
    # this only split for readability, could be combined with single mutations
    # redefine baujahr and letzte_modernisierung
    c("baujahr_cat") := lapply(.SD, function(x) {
      # sort into categories
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
        # label said categories
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

    # single mutations --------------------------------------------------------
    # objektzustand
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
      # ausstattung
      ausstattung = factor(
        ausstattung,
        0:4,
        c("MISSING",, "Simple", "Normal", "Sophisticated", "Deluxe")
      )
    )
  ] |> drop_na(ausstattung, baujahr_cat)


  

  return(RED_cleaned)
}
