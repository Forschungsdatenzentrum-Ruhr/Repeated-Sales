similarity_classification <- function(geo_grouped_data, curr_latlon_log) {
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
      
    } else {
      # this could be function since im doing it more than once
      # does this make the if within cluster-class irrelevant?
      clustering <- cluster$new(
        cluster_options = NULL
      )
      clustering$centers <- data.table(
        "counting_id" = as.numeric(unique_occurence_ids),
        "parent" = as.numeric(unique_occurence_ids),
        "sim_dist" = 0,
        "sim_index" = 0
      )
      
    }
    
    # remerge non-duplicates to clustering results
    id_key =  geo_grouped_data[clustering$centers, on = "counting_id"][,counting_id := NULL]
    
    # should this also allow deviations?
    id_combinations = combinations[id_key,
                                   on = .(wohnflaeche,zimmeranzahl,etage), 
                                   allow.cartesian = TRUE]
    
    # check if everything has atleast been chosen once
    tar_assert_true(nrow(id_combinations) >= nrow(geo_grouped_data), msg = glue::glue("Underspecfication produced in {first(id_combinations$counting_id)}"))
    
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
    
    # Unit-Test ---------------------------------------------------------------
    tar_assert_true(nrow(clustering$centers) == nrow(geo_grouped_data), msg = glue::glue("Similarity classification produced unequal rows: {head(geo_grouped_data$counting_id)}"))
    
    # merge cluster results to inital data and return
    out <- geo_grouped_data[
      clustering$centers,
      on = .(counting_id)
    ]
    

  return(out)
}
