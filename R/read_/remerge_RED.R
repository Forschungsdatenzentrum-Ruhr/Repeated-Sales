remerge_RED <- function(classification = NA, RED_all_columns = NA) {
  # find all columns that are present in both, classification columns take precedence
  # disregarding counting_id, since this is used for the merge
  names_diff <- setdiff(intersect(names(classification), names(RED_all_columns)), "counting_id")

  # remove columns from RED and merge classifcation
  RED_classified <- RED_all_columns[, !..names_diff][classification, on = "counting_id"]

  check_geo = RED_classified[,uniqueN(latlon_utm) == 1 & uniqueN(kid2019) == 1 , by = "parent"]
  tar_assert_true(all(check_geo[[2]]))
  
  # subset to 15 biggest cities
  big_fifteen = c(
    "11000000", # Berlin
    "02000000", # Hamburg
    "09162000", # München
    "05315000", # Köln
    "06412000", # Frankfurt
    "08111000", # Stuttgart
    "05111000", # Düsseldorf
    "14713000", # Leipzig
    "05913000", # Dortmund
    "05113000", # Essen
    "04011000", # Bremen
    "14612000", # Dresden
    "03241001", # Hannover
    "09564000", # Nürnberg
    "05112000"  # Duisburg
  ) |> as.numeric()

  RED_classified = RED_classified[gid2019 %in% big_fifteen]

  return(RED_classified)
}
