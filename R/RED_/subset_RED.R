subset_RED = function(RED_classified){
  
  
  # variant 1 -> keep only last spell
  last_spell = ave(1:length(RED_classified$obid), RED_classified$obid, FUN = length)
  RED_subset = RED_classified[spell == last_spell]
  tar_assert_true(sum(duplicated(RED$obid)) == 0)
  
  
  
  # # variant 2 -> keep only sold repeats
  # RED_subset = RED_classified[non_list_reason == "Sold"]
  
  return(RED_subset)
}