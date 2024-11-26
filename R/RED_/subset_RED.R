subset_RED <- function(RED_classified) {
  #' @title Subset RED data for analysis
  #'
  #' @description Subset RED data for analysis by keeping only the last spell of each observation.
  #' @param RED_classified data.table. RED data set with classification data.
  #'
  #' @return data.table. RED data set with only the last spell of each observation or only sold listings.
  #'
  #' @author Thorben Wiebe
  #----------------------------------------------
  # Input validation
  input_check(RED_classified, "data.table")
  #----------------------------------------------

  # # variant 1 -> keep only last spell
  # last_spell = ave(1:length(RED_classified$obid), RED_classified$obid, FUN = length)
  # RED_subset_classified = RED_classified[spell == last_spell]
  # # figure out why this returns TRUE for 17 obs
  # #tar_assert_true(!any(duplicated(RED_subset_classified$obid)))


  # variant 2 -> keep only sold repeats
  RED_subset_classified <- RED_classified[non_list_reason == "Sold"]

  #----------------------------------------------
  # Unit test
  empty_check(RED_subset_classified)
  #----------------------------------------------

  return(RED_subset_classified)
}
