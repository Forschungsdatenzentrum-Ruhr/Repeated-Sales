make_hedonic_WK <- function(RED_classified = NA, data_type = NA) {
  # find a more elegant solution to handeling the different data_types
  if (data_type != "WK") {
    # cli::cli_alert_info("Data type not applicable. Skipping...")

    tar_assert_true(F, "Data type not applicable. Stopping")
  }

  # Definitions -------------------------------------------------------------

  depVar <- "ln_flatprice_sqm"
  indepVar <- c(
    # cleaned/created during all_type_cleaning
    "baujahr_cat",
    "first_occupancy",
    "balkon",
    "garten",
    "einbaukueche",
    "gaestewc",
    "zimmeranzahl",
    "aufzug",
    "keller",
    "ausstattung",
    "betreut",
    "declared_wohngeld",
    "num_floors",
    "cat_floors"
  )
  fixed_effects <- c("kid2019", "ejahr")

  # depVar ------------------------------------------------------------------
  RED_classified[, "ln_flatprice_sqm" := log(kaufpreis / wohnflaeche)]


  # indepVar ----------------------------------------------------------------

  var_to_keep <- c(intersect(names(RED_classified), c(indepVar, depVar, fixed_effects)),"baujahr","letzte_modernisierung","objektzustand","anzahletagen","etage","wohngeld","wohnflaeche","kaufpreis")
  # clean data with procedure identical for all data_types
  RED_cleaned <- all_type_cleaning(
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
  #   * Apartment rents
  # this is exclusive in REDX and inclusive here
  upper_percentile <- quantile(RED_cleaned[wohnflaeche >= 0, wohnflaeche], 1 - (0.5 / 100))
  lower_percentile <- quantile(RED_cleaned[wohnflaeche >= 0, wohnflaeche], (0.5 / 100))

  RED_filtered <- RED_cleaned[
    zimmeranzahl < 8 &
      kaufpreis %between% c(0, 2000000) &
      wohnflaeche %between% c(lower_percentile, upper_percentile)
  ]

  rhs <- indepVar |> paste(collapse = " + ")
  f <- sprintf("%s ~ %s | %s", depVar, rhs, paste0(fixed_effects, collapse = "^")) |>
    as.formula()

  hedonic_WK <- feols(f, RED_filtered, combine.quick = F, mem.clean = T)
  summary(hedonic_WK)
  return(hedonic_WK)
}
