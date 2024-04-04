subset_RED = function(RED_classified){
  

  # # variant 1 -> keep only last spell
  # last_spell = ave(1:length(RED_classified$obid), RED_classified$obid, FUN = length)
  # RED_subset_classified = RED_classified[spell == last_spell]
  # # figure out why this returns TRUE for 17 obs
  # #tar_assert_true(!any(duplicated(RED_subset_classified$obid)))
  

  # variant 2 -> keep only sold repeats
  RED_subset_classified = RED_classified[non_list_reason == "Sold"]
  
  return(RED_subset_classified)
}