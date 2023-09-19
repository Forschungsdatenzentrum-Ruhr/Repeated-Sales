remerge_RED = function(classification = NA, RED_all_columns = NA){
  
  # find all columns that are present in both, classification columns take precedence
  # disregarding counting_id, since this is used for the merge
  names_diff = setdiff(intersect(names(classification),names(RED_all_columns)),"counting_id")
    
  # remove columns from RED and merge classifcation
  RED_classified = RED_all_columns[,!..names_diff][classification, on = "counting_id"]
  
  return(RED_classified)
}

