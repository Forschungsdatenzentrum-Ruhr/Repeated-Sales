make_similarity_lists <- function(combinations = NA, occurence_ids = NA) {
  #' @title WIP
  #'
  #' @description WIP
  #' @param WIP
  #' @param WIP
  #' @note
  #'
  #' @return WIP
  #' @author Thorben Wiebe
  #----------------------------------------------

  # declare lists to filled during loop
  similarity_index_list <- similarity_dist_list <- list()

  # increase etage by one to avoid scaling around 0
  combinations[, etage := etage + 1]

  ## similiarity calculations
  for (i in 1:nrow(combinations)) {
    # percentage of rooms scaled values are allowed to be off
    # e.g. what percentage of 8 rooms is 0.5 rooms
    # this feels way to complicated
    scaled_zimmeranzahl_r_o <- combinations[
      ,
      (as.numeric(combinations[i, "zimmeranzahl"]) + zimmeranzahl_r_o) / as.numeric(combinations[i, "zimmeranzahl"]) - 1
    ]
    # scaling around 0 causes div by 0 and inf for everything else
    # just increase etage by one? other solution?
    data_to_similarity <- scale(combinations, center = F, scale = combinations[i]) |> as.data.table()

    similarity_index_list[[i]] <- data_to_similarity[
      ,
      .(fcase(

        ## exact repeat [small percentage deviation acceptable]
        abs(1 - wohnflaeche) <= wohnflaeche_e_o &
          # zimmeranzahl and etage are exact matches
          zimmeranzahl == 1 &
          etage == 1,
        0,

        ## similar repeat [larger percentage deviation acceptable]
        abs(1 - wohnflaeche) <= wohnflaeche_r_o &
          # zimmeranzahl deviation acceptable / etage arbitrary
          abs(1 - zimmeranzahl) <= scaled_zimmeranzahl_r_o &
          # etage exact match
          etage == 1,
        1,

        # no matches
        default = NA
      ))
    ] |> as.matrix()

    similarity_dist_list[[i]] <- as.matrix(dist(data_to_similarity, method = "euclidean"))[i, ]
  }

  ## clustering
  # transform to data.tables and set counting ids as column names
  similarity_dist_list <- as.data.table(similarity_dist_list)
  similarity_index_list <- as.data.table(similarity_index_list)

  # enforce zero diagonal(allows classification of zero observations which otherwise are fully NA)
  diag(similarity_index_list) <- 0

  setnames(similarity_index_list, as.character(occurence_ids))
  setnames(similarity_dist_list, as.character(occurence_ids))


  return(list(similarity_index_list, similarity_dist_list))
}
