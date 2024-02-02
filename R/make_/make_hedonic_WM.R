make_hedonic_WM <- function(RED_classified = NA) {
  # Definitions -------------------------------------------------------------
  depVar <- "ln_rent_sqm"
  indepVar <- c(

    # raw
    "balkon",
    "garten",
    "einbaukueche",
    "gaestewc",
    "aufzug", # this isnt in REDX; why?
    "keller",
    "ausstattung",
    "zimmeranzahl",

    # mutated
    "baujahr_cat",
    "first_occupancy"
  )
  fixed_effects <- c("gid2019", "ejahr")

  # depVar ------------------------------------------------------------------
  RED_classified[mietekalt < 0, mietekalt := 0]
  RED_classified[, "ln_rent_sqm" := log(mietekalt / wohnflaeche)]


  # indepVar ----------------------------------------------------------------
  var_to_keep <- c(
    intersect(
      names(
        RED_classified
      ), c(indepVar, depVar, fixed_effects)
    ),
    "baujahr", # -> becomes 'baujahr_cat'
    "letzte_modernisierung",
    "objektzustand",
    "wohnflaeche", # used during outlier removal
    "rs_id",
    "emonths",
    "counting_id"
  )
  # drop extreme values of variables
  # this is exclusive in REDX and inclusive here
  upper_percentile <- quantile(RED_classified[wohnflaeche >= 0, wohnflaeche], 1 - (0.5 / 100))
  lower_percentile <- quantile(RED_classified[wohnflaeche >= 0, wohnflaeche], (0.5 / 100))

  # clean data with procedure identical for all data_types
  RED_WM <- all_type_cleaning(
    # drop everything else to reduce RAM usage
    RED_classified[zimmeranzahl < 8 &
      mietekalt %between% c(0, 5000) &
      wohnflaeche %between% c(lower_percentile, upper_percentile), ..var_to_keep],
    var_to_replace_missings = c(
      "balkon",
      "garten",
      "einbaukueche",
      "gaestewc",
      "aufzug",
      "keller",
      "ausstattung"
    )
  )

  return(RED_WM)
}
