make_hybrid <- function(RED_classified, prepared_repeated, data_type) {
  # build by me based on Case and Quigley 1991
  
  # # Debugging:
  # tar_load(WK_classified)
  # RED_classified = WK_classified; rm(WK_classified)
  # tar_load(WK_prepared_repeated)
  # prepared_repeated = WK_prepared_repeated; rm(WK_prepared_repeated)
  # data_type ="WK"


  # prep, get some settings
  list_var <- make_var(data_type = data_type)
  depVar <- list_var$depVar
  fixed_effects <- list_var$fixed_effects
  binary_names = list_var$binary_names
  cont_names = list_var$cont_names

  # declare variables to keep
  var_to_keep <- c(binary_names, cont_names, "rs_id", "emonths", "depVar", "counting_id")

  # get ids of all listings that are classified as repeat sales (pure or changed)
  all_rs <- prepared_repeated[["rs_id"]] |> unique()
  # split into repeat and hedonic
  # something in here causes a data.table warning: Invalid .internal.selfref detected 
  RED_classified <- prepare_hedonic(RED_classified, data_type)[, ":="(
    hybrid_type = fifelse(rs_id %in% all_rs, "repeat", "hedonic"),
    depVar = exp(get(depVar))
  )]

  # to split repeat into pure and changed, figure out which listings have changed within id
  # this means however that between pairs quality changed, so for that listing pair

  # reduce listings to only repeats and set missings to zero
  pure_rs <- RED_classified[
    hybrid_type == "repeat",
    ..var_to_keep
  ]

  changed_boolean <- pure_rs[,
    lapply(.SD, function(x) {
      c(NA, diff(x))
    }),
    by = rs_id,
    .SDcols = setdiff(var_to_keep, c("rs_id", "emonths", "depVar", "counting_id"))
  ][, rs_id := NULL] |> rowSums() != 0

  is.na(changed_boolean) <- FALSE

  # Unit test
  tar_assert_true(length(changed_boolean) == nrow(pure_rs), "Length of changed_boolean does not match pure_rs")
  
  pure_rs[, changed_to := changed_boolean][, changed_from := lead(changed_to, 1), by = rs_id]


  # this pretty much allows for duplicate indiviudal listings between pure/changed
  # sample 1 pure rs
  pure_pairs <- pure_rs[changed_to == FALSE | changed_from == FALSE]

  # sample 2 quality changed rs
  changed_pairs <- pure_rs[changed_to == TRUE | changed_from == TRUE]

  # smaple 3 hedonic
  hedonic_listings <- RED_classified[hybrid_type == "hedonic", ..var_to_keep]

  # type specific setups, mostly for readability
  # hedonic
  hedonic_V <- hedonic_listings[["depVar"]]
  hedonic_t_month <- hedonic_listings[["emonths"]]
  hedonic_counting_id <- hedonic_listings[["counting_id"]]

  # pure
  pure_V_t <- pure_pairs[["depVar"]]
  pure_V_T <- pure_pairs[, lag(depVar, 1), by = "rs_id"][, rs_id := NULL][["V1"]]
  pure_t_month <- pure_pairs[["emonths"]]
  pure_T_month <- pure_pairs[, lag(emonths, 1), by = "rs_id"][, rs_id := NULL][["V1"]]
  pure_counting_id <- pure_pairs[["counting_id"]]
  
  # changed
  changed_V_t <- changed_pairs[["depVar"]]
  changed_V_T <- changed_pairs[, lag(depVar, 1), by = "rs_id"][, rs_id := NULL][["V1"]]
  changed_t_month <- changed_pairs[["emonths"]]
  changed_T_month <- changed_pairs[, lag(emonths, 1), by = "rs_id"][, rs_id := NULL][["V1"]]
  changed_counting_id <- changed_pairs[["counting_id"]]

  # build Z using X_1, X_2, X_3
  Z <- do.call(rbind, list(
    # hedonic
    X_1 = make_X_1(hedonic = hedonic_listings, x_conts = cont_names, x_binaries = binary_names, t_month = hedonic_t_month),
    # pure
    X_2 = make_X_2(pure = pure_pairs, x_conts = cont_names, x_binaries = binary_names, t_month = pure_t_month, T_month = pure_T_month),
    # changed
    X_3 = make_X_3(
      changed = changed_pairs,
      x_conts = cont_names,
      x_binaries = binary_names,
      t_month = changed_t_month,
      T_month = changed_T_month
    )
  ))
  # build Y using price variables
  Y <- log(
    c(
      hedonic_V,
      (pure_V_t / pure_V_T),
      (changed_V_t / changed_V_T)
    )
  )
  # combine Z and Y and add counting_id
  combined_hybrid <- cbind(Z, Y)[, counting_id := c(hedonic_counting_id, pure_counting_id, changed_counting_id)] |> na.omit()

  # remerge fixed effects -> used for the pindex
  var_to_keep <- c(fixed_effects, "counting_id")
  combined_hybrid <- combined_hybrid[RED_classified[, .SD, .SDcols = var_to_keep], on = c("counting_id")]

  # final clean up -> these shouldnt really happend beforehand
  combined_hybrid <- combined_hybrid[pre_zimmeranzahl != -Inf & sub_zimmeranzahl != -Inf & Y > 0]

  # run regression
  hybrid_regression <- lm(Y ~ ., data = combined_hybrid)
  
  pindex = (exp(predict(hybrid_regression))-1)*100

  # add pindex to datas
  combined_hybrid = combined_hybrid[, .(index = pindex, counting_id)]
  out <- RED_classified[combined_hybrid, on = "counting_id"]

  #----------------------------------------------
  return(out)
}
