make_hedonic_WK <- function(RED_classified = NA) {
  # Definitions -------------------------------------------------------------
  depVar <- "ln_flatprice_sqm"
  indepVar <- c(

    # raw
    "balkon",
    "garten",
    "einbaukueche",
    "gaestewc",
    "aufzug",
    "keller",
    "betreut",
    "ausstattung",
    "zimmeranzahl",

    # mutated
    "declared_wohngeld",
    "baujahr_cat",
    "first_occupancy",
    "num_floors",
    "floors_cat"
  )
  fixed_effects <- c("kid2019", "ejahr")

  # depVar ------------------------------------------------------------------
  RED_classified[kaufpreis < 0, kaufpreis := 0]
  RED_classified[, "ln_flatprice_sqm" := log(kaufpreis / wohnflaeche)]


  # indepVar ----------------------------------------------------------------
  var_to_keep <- c(
    intersect(
      names(RED_classified),
      c(indepVar, depVar, fixed_effects)
    ),
    "baujahr", # -> becomes 'baujahr_cat'
    "objektzustand", # becomes -> "first_occupancy"
    "anzahletagen", # -> becomes 'baujahr_cat'
    "etage", # -> becomes 'baujahr_cat'
    "wohngeld", # -> becomes 'baujahr_cat'
    "wohnflaeche" # used during outlier removal
  )
  # clean data with procedure identical for all data_types
  RED_WK <- all_type_cleaning(
    # drop everything else to reduce RAM usage
    RED_classified[, ..var_to_keep],
    var_to_replace_missings = c(
      "balkon",
      "garten",
      "einbaukueche",
      "gaestewc",
      "aufzug",
      "keller",
      "ausstattung",
      "betreut"
    ),
    indepVar
  )

  # drop extreme values of variables
  # this is exclusive in REDX and inclusive here
  upper_percentile <- quantile(RED_WK[wohnflaeche >= 0, wohnflaeche], 1 - (0.5 / 100))
  lower_percentile <- quantile(RED_WK[wohnflaeche >= 0, wohnflaeche], (0.5 / 100))

  RED_WK[
    zimmeranzahl < 8 &
      kaufpreis %between% c(0, 2000000) &
      wohnflaeche %between% c(lower_percentile, upper_percentile),
    ":="(
      # wohngeld
      declared_wohngeld = fifelse(between(wohngeld, 0, 2500), "Yes", "No"),
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
      floors_cat = fcase(
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
      )
    )
  ]

  tar_assert_true(all(indepVar %in% names(RED_WK)))

  rhs <- indepVar |> paste(collapse = " + ")
  f <- sprintf("%s ~ %s | %s", depVar, rhs, paste0(fixed_effects, collapse = "^")) |>
    as.formula()

  hedonic_WK <- feols(f, RED_WK, combine.quick = F, mem.clean = T)

  return(hedonic_WK)
}
