make_hedonic_WM <- function(RED_classified) {
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
  # Input validation
  input_check(RED_classified, "data.table")
  #----------------------------------------------

  # setup of regression
  list_var <- make_var_list(data_type = "WM")
  depVar <- list_var$depVar
  indepVar <- list_var$indepVar
  fixed_effects <- list_var$fixed_effects

  # depVar prep
  RED_classified[mietekalt < 0, mietekalt := 0]
  RED_classified[, "ln_rent_sqm" := log(mietekalt / wohnflaeche)]

  # indepVar prep
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


  # do rule based cleanup and drop all unsed variables to reduce RAM
  RED_classified = RED_classified[zimmeranzahl < 8 &
      mietekalt %between% c(0, 5000) &
      wohnflaeche %between% c(lower_percentile, upper_percentile), ..var_to_keep]

  # clean data with procedure identical for all data_types
  RED_WM <- all_type_cleaning(
    RED_classified,
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
  #----------------------------------------------
  # Unit test
  empty_check(RED_WM)
  #----------------------------------------------
  return(RED_WM)
}
