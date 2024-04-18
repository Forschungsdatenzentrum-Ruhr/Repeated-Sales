similarity_classification <- function(geo_grouped_data, curr_latlon_log) {
    #' @title Make similarity classification
    #'
    #' @description This function classifies listings based on their similarity
    #' @param geo_grouped_data data.table. Data set with grouped data
    #' @param curr_latlon_log character. Current latlon log for logging
    #'
    #' @return data.table. Data set with classified listings
    #' @author Thorben Wiebe
    #----------------------------------------------
    # Input validation
    input_check(geo_grouped_data, "data.table")
    input_check(curr_latlon_log, "character")
    #----------------------------------------------
    #log info for debugging
    logger::log_info(glue::glue("Working on {curr_latlon_log}; N = {nrow(geo_grouped_data)}"))

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
      # NOTE: this could be a class since it has a lot of attributes
      # NOTE: consider swapping these for sparse matrices to save memory -> should be straightforward, since (when dropping resembling matches) there are only 0/1 values

      # make similarity lists
      similarity_lists = make_similarity_lists(unique_combinations,unique_occurence_ids)
      
      # split lists into index and distance for clustering and clarity
      similarity_index_list = similarity_lists[[1]]
      similarity_dist_list = similarity_lists[[2]]
      
      # exclude similarity distances irrelevant by similarity index (values are not in interval)
      # this reduces computation substantially since we don't have to compare n x n each time but n x m instead (n > m)
      similarity_dist_list[is.na(similarity_index_list)] = NA

      # setup and run the actual clustering
      clustering <- cluster$new(
        cluster_options = similarity_index_list,
        distance = similarity_dist_list
      )
      # run clustering and determine cluster centroids, modifies clustering$centers
      # NOTE: think about naming conventions, parent vs centroid vs center and child vs member
      clustering$determine_cluster_centers()
      
    } else {
      # NOIE: does this make the if within cluster-class irrelevant?
      clustering <- cluster$new(
        cluster_options = NULL
      )
      # self assign as parent to avoid missings at this stage
      clustering$centers <- data.table(
        "counting_id" = as.numeric(unique_occurence_ids),
        "parent" = as.numeric(unique_occurence_ids),
        "sim_dist" = 0,
        "sim_index" = 0
      )
      
    }
    
    # remerge non-duplicates to clustering results
    id_key =  geo_grouped_data[clustering$centers, on = "counting_id"][,counting_id := NULL]
    
    # NOTE: should this also allow deviations?
    id_combinations = combinations[id_key,
                                   on = .(wohnflaeche,zimmeranzahl,etage), 
                                   allow.cartesian = TRUE]
    
    # Unit test: check if everything has atleast been chosen once
    tar_assert_true(nrow(id_combinations) >= nrow(geo_grouped_data), msg = glue::glue("Underspecfication produced in {first(id_combinations$counting_id)}"))
    
    # reassign to clustering for further processing
    clustering_names = names(clustering$centers)
    clustering$centers = id_combinations[, ..clustering_names]
    
    
    if (anyDuplicated(clustering$centers$counting_id)) {
      # filter/fix duplicates within $centers here if they exist
      clustering$centers <- clustering$centers[
        ,
        similarity_cost_function(.SD)
      ]
    }

    # Unit-Test: check if all rows have been chosen
    tar_assert_true(nrow(clustering$centers) == nrow(geo_grouped_data), msg = glue::glue("Similarity classification produced unequal rows: {head(geo_grouped_data$counting_id)}"))
    
    # merge cluster results to inital data and return
    out <- geo_grouped_data[
      clustering$centers,
      on = .(counting_id)
    ]
    # ----------------------------------------------
    # Unit test
    empty_check(out)
    #----------------------------------------------
  return(out)
}
