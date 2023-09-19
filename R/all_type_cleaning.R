all_type_cleaning <- function(RED_classified = NA, var_to_replace_missings = NA, indepVar = NA) {
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
    c("baujahr_cat", "modernisierung_cat") := lapply(.SD, function(x) {
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
            NA_character_,
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
    .SDcols = c("baujahr", "letzte_modernisierung")
  ][

    # single mutations --------------------------------------------------------
    # objektzustand
    , ":="(
      first_occupancy = fifelse(
        objektzustand == 1, "Yes", "No"
      ),
      # ausstattung
      ausstattung = factor(
        ausstattung,
        0:4,
        c(NA_character_, "Simple", "Normal", "Sophisticated", "Deluxe")
      ),
      ## etagen
      # anzahletagen
      num_floors = fcase(
        anzahletagen <= 0, 0,
        between(anzahletagen, 1, 3), 1,
        between(anzahletagen, 4, 5), 2,
        between(anzahletagen, 6, 10), 3,
        anzahletagen > 10, 4
      ) |> factor(
        0:4,
        c(
          NA_character_,
          "1-3 floors",
          "4-5 floors",
          "6-10 floors",
          "more than 10 floors"
        )
      ),
      # category etagen
      cat_floors = fcase(
        etage < 0, 0,
        etage == 0, 1,
        etage == 1, 2,
        between(etage, 2, 3), 3,
        between(etage, 4, 5), 4,
        between(etage, 6, 10), 5,
        anzahletagen > 10, 6
      ) |> factor(
        0:6,
        c(
          NA_character_,
          "ground floor (UG)",
          "first floor (EG)",
          "2nd to 3rd floor",
          "4th to 5th floor",
          "6th to 10th floor",
          "above 10th floor"
        )
      ),
      # wohngeld
      declared_wohngeld = fifelse(between(wohngeld, 0, 2500), "Yes", "No")
    )
  ]


  tar_assert_true(all(indepVar %in% names(RED_cleaned)))

  return(RED_cleaned)
}
