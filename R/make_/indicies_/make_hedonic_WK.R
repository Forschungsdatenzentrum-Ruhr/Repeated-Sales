make_hedonic_WK <- function(RED_classified) {
  #' @title WIP
  #'
  #' @description WIP
  #' @param WIP
  #' @param WIP
  #' @note
  #'
  #' @return WIP
  #' @author Thorben Wiebe
  #----------------------------------------------

  # setup of regression
  list_var <- make_var(data_type = "WK")
  depVar <- list_var$depVar
  indepVar <- list_var$indepVar
  fixed_effects <- list_var$fixed_effects

  # depVar prep
  RED_classified[kaufpreis < 0, kaufpreis := 0]
  RED_classified[, "ln_flatprice_sqm" := log(kaufpreis / wohnflaeche)]


  # indepVar prep
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
    "wohnflaeche", # used during outlier removal
    "rs_id",
    "emonths",
    "counting_id"
  )
  # drop extreme values of variables
  # this is exclusive in REDX and inclusive here
  upper_percentile <- quantile(RED_classified[wohnflaeche >= 0, wohnflaeche], 1 - (0.5 / 100))
  lower_percentile <- quantile(RED_classified[wohnflaeche >= 0, wohnflaeche], (0.5 / 100))


  # do rule based cleanup and drop all unsed variables to reduce RAM
  RED_classified = RED_classified[zimmeranzahl < 8 &
      kaufpreis %between% c(0, 2000000) &
      wohnflaeche %between% c(lower_percentile, upper_percentile), ..var_to_keep]

  # clean data with procedure identical for all data_types
  RED_WK <- all_type_cleaning(
    RED_classified,
    var_to_replace_missings = c(
      "balkon",
      "garten",
      "einbaukueche",
      "gaestewc",
      "aufzug",
      "keller",
      "ausstattung",
      "betreut"
    )
  )

  # type specific mutations
  RED_WK = RED_WK[,
    ":="(
      # wohngeld
      declared_wohngeld = fifelse(between(wohngeld, 0, 2500), 1, 0) |> factor(
        0:1,
        c(
          "Yes",
          "No"
        )
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
          "MISSING",
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
          "MISSING",
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

  #----------------------------------------------
  return(RED_WK)
}
