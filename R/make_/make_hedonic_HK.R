make_hedonic_HK <- function(RED_classified = NA) {
  # Definitions -------------------------------------------------------------
  depVar <- "ln_houseprice_sqm"

  indepVar <- c(

    # raw
    "gaestewc",
    "einliegerwohnung",
    "ausstattung",
    "zimmeranzahl",

    # mutated
    "baujahr_cat",
    "first_occupancy",
    "plotarea_cat",
    "type_cat"
  )
  fixed_effects <- c("gid2019", "ejahr")

  # depVar ------------------------------------------------------------------
  RED_classified[kaufpreis < 0, kaufpreis := 0]
  RED_classified[, "ln_houseprice_sqm" := log(kaufpreis / wohnflaeche)]


  # indepVar ----------------------------------------------------------------
  var_to_keep <- c(
    intersect(
      names(RED_classified),
      c(indepVar, depVar, fixed_effects)
    ),
    "baujahr", # -> becomes 'baujahr_cat'
    "objektzustand", # -> becomes 'first_occupancy'
    "grundstuecksflaeche", # -> becomes 'plotarea_cat'
    "kategorie_Haus", # -> becomes 'type_cat'
    "wohnflaeche", # used during outlier removal
    "rs_id",
    "emonths",
    "counting_id"
  )
  # TODO: make the cutting a function and equal for all -> do it after merging?
  # drop extreme values of variables
  # this is exclusive in REDX and inclusive here
  upper_percentile <- quantile(RED_classified[wohnflaeche >= 0, wohnflaeche], 1 - (0.5 / 100))
  lower_percentile <- quantile(RED_classified[wohnflaeche >= 0, wohnflaeche], (0.5 / 100))
  
  # clean data with procedure identical for all data_types
  RED_HK <- all_type_cleaning(
    # drop everything else to reduce RAM usage
    RED_classified[
      zimmeranzahl < 15 &
        kaufpreis %between% c(0, 5000000) &
        wohnflaeche %between% c(lower_percentile, upper_percentile),
      grundstuecksflaeche > 2500, ..var_to_keep],
    var_to_replace_missings = c(
      "gaestewc",
      "ausstattung"
    )
  )

  RED_HK = RED_HK[,
    ":="(
      plotarea_cat = fcase(
        grundstuecksflaeche <= 0, 0,
        between(grundstuecksflaeche, 1, 200), 1,
        between(grundstuecksflaeche, 201, 400), 2,
        between(grundstuecksflaeche, 401, 600), 3,
        between(grundstuecksflaeche, 601, 800), 4,
        between(grundstuecksflaeche, 801, 1200), 5,
        grundstuecksflaeche > 1201, 6
      ) |> factor(
        0:6,
        c(
          NA_character_,
          "(0-200]",
          "(200-400]",
          "(400-600]",
          "(600-800]",
          "(800-1200]", 
          ">1200"
        )
      ),
      type_cat = fcase(
        kategorie_Haus < 0, 0,
        kategorie_Haus %in% c(1,7,8), 1,
        kategorie_Haus %in% c(2,3), 2,
        kategorie_Haus %in% c(4,5,6), 3,
        kategorie_Haus %in% c(9,10), 4,
        kategorie_Haus %in% c(11,12), 5,
        TRUE, 6
      ) |> factor(
        0:7,
        c(
          NA_character_,
          "typ_freistehend",
          "typ_DHH",
          "typ_Reihenhaus",
          "typ_exclusive",
          "typ_MFH",
          "typ_other"
        )
      )
    )
  ]

  return(RED_HK)
}

