make_hybrid <- function(RED_subset_classified, prepared_repeated, data_type) {
  #' @title Make Hybrid Index
  #'
  #' @description Make the hybrid index for the given data type
  #' @param RED_subset_classified data.table. Classified RED data
  #' @param prepared_repeated data.table. Prepared repeated data
  #' @param data_type character. Data type of the classified RED data
  #'
  #' @note Build by me based on Case and Quigley 1991
  #'
  #' @return data.table. Hybrid index for the given data type
  #' @author Thorben Wiebe
  #----------------------------------------------
  # Input validation
  input_check(RED_subset_classified, "data.table")
  input_check(prepared_repeated, "data.table")
  input_check(data_type, "character")
  #----------------------------------------------
  # prep, get some settings
  list_var <- make_var_list(data_type = data_type)
  depVar <- list_var$depVar
  fixed_effects <- list_var$fixed_effects
  binary_names <- list_var$binary_names
  cont_names <- list_var$cont_names
  indepVar <- c(paste0("pre_", c(cont_names, binary_names)), paste0("sub_", c(cont_names, binary_names)))

  # declare variables to keep
  var_to_keep <- c(binary_names, cont_names, "rs_id", "emonths", "depVar", "counting_id")

  # get ids of all listings that are classified as repeat sales (pure or changed)
  all_rs <- prepared_repeated[["rs_id"]] |> unique()
  # split into repeat and hedonic
  RED_subset_classified <- prepare_hedonic(RED_subset_classified, data_type)[, ":="(
    hybrid_type = fifelse(rs_id %in% all_rs, "repeat", "hedonic"),
    depVar = exp(get(depVar))
  )]
  tar_assert_true(all(c("hybrid_type", "depVar") %in% names(RED_subset_classified)), msg = "Missing variables")

  # to split repeat into pure and changed, figure out which listings have changed within id
  # this means however that between pairs quality changed, so for that listing pair

  # reduce listings to only repeats and set missings to zero
  pure_rs <- RED_subset_classified[
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
  hedonic_listings <- RED_subset_classified[hybrid_type == "hedonic", ..var_to_keep]

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
  combined_hybrid <- cbind(Z, Y)[, ":="(counting_id = c(
    hedonic_counting_id,
    pure_counting_id, 
    changed_counting_id
  ),
  # testing 
  id_type = c(rep("hedonic", length(hedonic_counting_id)),
              rep("pure", length(pure_counting_id)),
              rep("changed", length(changed_counting_id))
              
              )
  )
  ] |> na.omit()
  custom_single_tabyl(combined_hybrid, "id_type", data_type)


  #combined_hybrid = combined_hybrid[id_type %in% c("changed")]

  # remerge fixed effects -> used for the pindex
  var_to_keep <- c(fixed_effects, "counting_id")
  combined_hybrid <- combined_hybrid[RED_subset_classified[, .SD, .SDcols = var_to_keep], on = "counting_id"]

  # final clean up
  combined_hybrid <- combined_hybrid[is.finite(pre_zimmeranzahl) & is.finite(sub_zimmeranzahl) & Y != 0]

  # construct regression formula
  f = regression_function(indepVar, "Y")
  hybrid_regression = lm(f, data = combined_hybrid)

  # export regression to modelsummary
  modelsummary::modelsummary(
    hybrid_regression,
    stars = c("***" = .01, "**" = .05, "*" = .1),
    fmt = 4,
    output = glue::glue("{output_path}/{data_type}/hybrid_regression.txt") # ,
    # gof_map = c("nobs", "r.squared", "adj.r.squared")
  )

  pindex = (exp(predict(hybrid_regression))-1) * 100

  combined_hybrid <- combined_hybrid[, .(index = pindex, counting_id)]
  out <- RED_subset_classified[combined_hybrid, on = "counting_id"]

  #----------------------------------------------
  # Unit test
  empty_check(out)
  #----------------------------------------------
  return(out)
}
