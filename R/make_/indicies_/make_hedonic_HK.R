make_hedonic_HK <- function(RED_classified) {
  #' @title Make Hedonic Index for HK
  #'
  #' @description Make the hedonic index for the HK data type
  #' @param RED_classified data.table. Classified RED data
  #' 
  #' @return data.table. Hedonic index for HK data type
  #' @author Thorben Wiebe
  #----------------------------------------------
  # Input validation
  input_check(RED_classified, "data.table")
  #----------------------------------------------
  
  # setup of regression
  list_var <- make_var_list(data_type = "HK")
  depVar <- list_var$depVar
  indepVar <- list_var$indepVar
  fixed_effects <- list_var$fixed_effects

  # depVar prep
  RED_classified[kaufpreis < 0, kaufpreis := 0]
  RED_classified[, "ln_houseprice_sqm" := log(kaufpreis / wohnflaeche)]

  # indepVar prep
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
  
  # drop extreme values of variables
  # this is exclusive in REDX and inclusive here
  
  
  # do rule based cleanup and drop all unsed variables to reduce RAM
  RED_classified = RED_classified[,..var_to_keep]


  # clean data with procedure identical for all data_types
  RED_HK <- all_type_cleaning(
    RED_classified,
    var_to_replace_missings = c(
      "gaestewc",
      "ausstattung"
    )
  )[,
    ":="(
      plotarea_cat = fcase(
        grundstuecksflaeche <= 0 | is.na(grundstuecksflaeche), 0,
        between(grundstuecksflaeche, 1, 200), 1,
        between(grundstuecksflaeche, 201, 400), 2,
        between(grundstuecksflaeche, 401, 600), 3,
        between(grundstuecksflaeche, 601, 800), 4,
        between(grundstuecksflaeche, 801, 1200), 5,
        grundstuecksflaeche > 1201, 6
      ) |> factor(
        0:6,
        c(
          "MISSING",
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
        default =  6
      ) |> factor(
        0:6,
        c(
          "MISSING",
          "typ_freistehend",
          "typ_DHH",
          "typ_Reihenhaus",
          "typ_exclusive",
          "typ_MFH",
          "typ_other"
        )
      )
    ) 
  ][!is.na(plotarea_cat)] # I have no idea how there could still be NAs
  #----------------------------------------------
  # Unit test
  empty_check(RED_HK)
  #----------------------------------------------
  return(RED_HK)
}

