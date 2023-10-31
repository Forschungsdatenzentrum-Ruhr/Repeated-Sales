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
  ## Preperation
  
  # make copy to modifiy keys
  geo_grouped_data <- copy(geo_grouped_data)
  setkey(geo_grouped_data, counting_id)
  setindex(geo_grouped_data, wohnflaeche, etage, zimmeranzahl)
  
  # extract ids and combinations of non-duplicates
  occurence_ids <- geo_grouped_data[, counting_id]
  
  # extract all combinations of categories
  combinations <- geo_grouped_data[, ..categories]
  
    if (!nrow(combinations) == 1) {
      
      # this could be a class
      similarity_lists = make_similarity_lists(combinations)
      
      similarity_index_list = similarity_lists[[1]]
      similarity_dist_list = similarity_lists[[2]]
      
      # exclude similarity distances maybe irrelevant by similarity index (values are not in interval)
      # this reduces computation substantially since we don't have to compare n x n each time but n x m instead (n > m)
      similarity_dist_list[is.na(similarity_index_list)] = NA
      
    # setup and run the actual clustering
    clustering <- cluster$new(
      cluster_options = similarity_index_list,
      distance = similarity_dist_list
    )
    clustering$determine_cluster_centers()
    clustering$centers
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
  tar_assert_true(nrow(clustering$centers) == nrow(geo_grouped_data), msg = head(geo_grouped_data$counting_id))

  # merge cluster results to inital data and return
  out <- geo_grouped_data[
    clustering$centers,
    on = .(counting_id)
  ]

  # make sure we dont output more obs than we input
  # tar_assert_true(nrow(geo_grouped_data) == nrow(out))

  # check if no NAs were created somewhere
  # tar_assert_true(!out[,anyNA(.SD), .SDcols = c("sim_index","sim_dist","parent")])

  return(out)
}
