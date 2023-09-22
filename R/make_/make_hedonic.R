make_hedonic = function(RED_classified = NA, data_type = NA){
  
  hedonic = fcase(
    data_type == "WK", make_hedonic_WK(RED_classified),
    data_type == "WM", make_hedonic_WM(RED_classified),
    data_type == "HK", make_hedonic_HK(RED_classified)
  )
  
  
}