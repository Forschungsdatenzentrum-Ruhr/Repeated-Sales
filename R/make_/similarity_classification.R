similarity_classification <- function(geo_grouped_data = NA, curr_latlon_log) {
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

# tryCatch(
#   {
    # make copy to modifiy keys
    geo_grouped_data <- copy(geo_grouped_data)
    setkey(geo_grouped_data, counting_id)
    setindex(geo_grouped_data, wohnflaeche, etage, zimmeranzahl)
    
    # extract ids and combinations of non-duplicates
    occurence_ids <- geo_grouped_data[, counting_id]
    
    # extract all combinations of categories
    var_to_keep = c(categories, "counting_id")
    combinations <- geo_grouped_data[, ..var_to_keep]

    # filter out duplicates
    # this has to be done ignoring the counting_id, as it is unique
    dup_combinations = duplicated(combinations[, ..categories])
    unique_combinations = combinations[!dup_combinations, ..categories]
    unique_occurence_ids = combinations[!dup_combinations, counting_id]
   
    if (!nrow(unique_combinations) == 1) {
      
      # this could be a class
      # consider swapping these for sparse matrices to save memory
      # should be straightforward, since (when dropping resembling matches) there are only 0/1 values
      similarity_lists = make_similarity_lists(unique_combinations,unique_occurence_ids)
      
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

      # remerge non-duplicates to clustering results
      # the idea here is to do the heavy lifting on as little data as possible
      # and then remerge the results to the original data
      id_key = clustering$centers[geo_grouped_data, on = "counting_id"][,counting_id:= NULL]

      # should this also allow deviations?
      id_combinations = combinations[id_key, on = .(wohnflaeche, etage, zimmeranzahl), allow.cartesian = TRUE]

      # check if all rows that were in the original data are still in the clustering results
      #tar_assert_true(nrow(clustering$centers[id_combinations, on = .(counting_id)]) == nrow(clustering$centers), msg = head(id_combinations))
      
      # reassign to clustering for further processing
      clustering_names = names(clustering$centers)
      clustering$centers = id_combinations[, ..clustering_names]

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
#   },
#   error = function(cond){
#     logger::log_info("Curr latlon_utm: {curr_latlon_log}")
#     logger::log_error("Errored:")
#     logger::log_error(conditionMessage(cond))
#     cli::cli_alert_info("Error log created. See log folder.")
#   },
#   warning = function(cond){
#     logger::log_info("Curr latlon_utm: {curr_latlon_log}")
#     logger::log_warn("Warning:")
#     logger::log_warn(conditionMessage(cond))
#     cli::cli_alert_info("Warn log created. See log folder.")
#   }
# )

  return(out)
}
