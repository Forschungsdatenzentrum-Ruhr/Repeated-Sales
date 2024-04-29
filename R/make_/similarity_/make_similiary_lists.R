make_similarity_lists <- function(combinations, occurence_ids) {
  #' @title Make similarity lists
  #'
  #' @description This function calculates the similarity index and distance for each combination. Helper function for similarity_classification
  #' @param combinations data.table. Data set with combinations of listings to be compared
  #' @param occurence_ids numeric. Vector with occurence ids
  #'
  #' @return list. List with similarity index and distance
  #' @author Thorben Wiebe
  #----------------------------------------------
  # Input validation
  input_check(combinations, "data.table")
  input_check(occurence_ids, "integer")
  #----------------------------------------------
  # declare lists to filled during loop
  similarity_index_list <- similarity_dist_list <- list()

  # increase etage by one to avoid scaling around 0
  # consider just increasing ground floor by one; see what changes
  combinations <- combinations[, etage := etage + 1]

  ## similiarity calculations
  for (i in 1:nrow(combinations)) {
    # percentage of rooms scaled values are allowed to be off
    # e.g. what percentage of 8 rooms is 0.5 rooms
    # NOTE: this feels way to complicated
    scaled_zimmeranzahl_e_o <- combinations[
      ,
      (as.numeric(combinations[i, "zimmeranzahl"]) + zimmeranzahl_e_o) / as.numeric(combinations[i, "zimmeranzahl"]) - 1
    ]
    # scale all columns by the current row
    # transforms values in percentage deviation from current row
    data_to_similarity <- scale(combinations, center = F, scale = combinations[i]) |> as.data.table()
    # calculate similarity index
    similarity_index_list[[i]] <- data_to_similarity[
      ,
      .(fcase(
        ## exact repeat [small percentage deviation acceptable]
        between_helper(1, wohnflaeche, wohnflaeche_e_o) &
          between_helper(1, zimmeranzahl, scaled_zimmeranzahl_e_o) &
          between_helper(1, etage, etage_e_o),
        0,
        # no matches
        default = NA
      ))
    ] |> as.matrix()
    # calculate similarity distance, i.e. euclidean distance between scaled values
    similarity_dist_list[[i]] <- as.matrix(dist(data_to_similarity, method = "euclidean"))[i, ]
  }

  # transform to data.tables and set counting ids as column names
  similarity_dist_list <- as.data.table(similarity_dist_list)
  similarity_index_list <- as.data.table(similarity_index_list)

  # enforce zero diagonal(allows classification of zero observations which otherwise are fully NA)
  diag(similarity_index_list) <- 0

  # set column names to occurence ids
  setnames(similarity_index_list, as.character(occurence_ids))
  setnames(similarity_dist_list, as.character(occurence_ids))

  #----------------------------------------------
  # Unit test
  empty_check(similarity_index_list)
  empty_check(similarity_dist_list)
  # check if column names match
  tar_assert_true(
    all(names(similarity_index_list) == names(similarity_dist_list)),
    msg = glue::glue("Column names of similarity index and distance do not match: {setdiff(names(similarity_index_list),names(similarity_dist_list))}")
  )
  # check if dimensions match
  tar_assert_true(
    all(dim(similarity_index_list) == dim(similarity_dist_list)),
    msg = glue::glue("Dimensions of similarity index and distance do not match: {dim(similarity_index_list)} vs {dim(similarity_dist_list)}")
  )
  #----------------------------------------------
  return(list(similarity_index_list, similarity_dist_list))
}
