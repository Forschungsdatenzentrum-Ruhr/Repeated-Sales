similarity_classification <- function(geo_grouped_data = NA) {
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

  # backup = geo_grouped_data

  # not working
  # geo_grouped_data = classification_77a61db3[latlon_utm == "5875229.20871175489859.109419092"]

  # example of overlapping parents/children
  # geo_grouped_data = RED |>  filter(latlon_utm == "5914872.28545209603584.936611244") |> setDT()

  ## Preperation

  # make copy to modifiy keys
  geo_grouped_data <- copy(geo_grouped_data)
  setkey(geo_grouped_data, counting_id)
  setindex(geo_grouped_data, wohnflaeche, etage, zimmeranzahl)

  # extract ids and combinations of non-duplicates
  occurence_ids <- geo_grouped_data[, counting_id]

  # extract all combinations of categories
  combinations <- geo_grouped_data[, ..categories]

  # declare lists to filled during loop
  similarity_index_list <- similarity_dist_list <- list()

  if (!nrow(combinations) == 1) {
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

    # setup and run the actual clustering
    clustering <- cluster$new(
      cluster_options = similarity_index_list,
      distance = similarity_dist_list,
      # redo this using selection instead of multiplication -> small deviations vansih overwise since they are multiplied by 0
      means = rowMeans(similarity_index_list * similarity_dist_list, na.rm = T)
    )
    clustering$determine_cluster_centers()

    if (anyDuplicated(clustering$centers$counting_id)) {
      # filter/fix duplicates within $centers here if they exist
      # currently its always being parents > being a child to ease calc
      # otherwise there has to be a cost assigned for non-parenthood
      # can prob just compare sim_dist to parent (gain from being child) vs
      # sum(sim_dist) to children (gain from being parent/cost)
      clustering$centers <- clustering$centers[
        ,
        similarity_cost_function(.SD)
      ]
    }
  } else {
    # this could be function since im doing it more than once
    # does this make the if within cluster-class irrelevant?
    clustering <- cluster$new(
      cluster_options = NULL
    )
    clustering$centers <- data.table(
      "counting_id" = as.numeric(occurence_ids),
      "parent" = as.numeric(occurence_ids),
      "sim_dist" = 0,
      "sim_index" = 0
    )
  }

  # Unit-Test ---------------------------------------------------------------
  # is it fine if clustering$centers is less?
  # 8948 16714 29616
  tar_assert_true(nrow(clustering$centers) == nrow(geo_grouped_data), msg = head(geo_grouped_data$counting_id))
  
  
  
  # merge cluster results to inital data and return
  out <- geo_grouped_data[
    clustering$centers,
    on = .(counting_id)
    # allow.cartesian = T
  ]

  # make sure we dont output more obs than we input
  # tar_assert_true(nrow(geo_grouped_data) == nrow(out))

  # check if no NAs were created somewhere
  # tar_assert_true(!out[,anyNA(.SD), .SDcols = c("sim_index","sim_dist","parent")])

  return(out)
}
