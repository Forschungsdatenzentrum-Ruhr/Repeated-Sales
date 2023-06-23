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
  #geo_grouped_data = federal_state |>  filter(latlon_utm == "5914872.28545209603584.936611244") |> setDT()

  ## Preperation
  
  # find duplicates
  duplicates <- duplicated(geo_grouped_data[, ..categories])

  # extract ids and combinations of non-duplicates
  first_occurence_ids <- geo_grouped_data[!duplicates, counting_id]
  
  # does order matter here? key = amonths?
  unique_combinations <- geo_grouped_data[!duplicates, ..categories]
  
  # assign id to exact duplicate combination based on first occurrence
  geo_grouped_data = copy(geo_grouped_data)[
    cbind(first_occurence_ids,unique_combinations),
    on = categories
  ]
  setkey(geo_grouped_data, wohnflaeche, etage, zimmeranzahl)
  
  similarity_index_list <- similarity_dist_list <- list()

if(!nrow(unique_combinations) == 1){

  # increase etage by one to avoid scaling around 0
  
  unique_combinations[, etage := etage + 1]
  
  
  ## similiarity calculations
  for (i in 1:nrow(unique_combinations)) {
    
    # percentage of rooms scaled values are allowed to be off
    # e.g. what percentage of 8 rooms is 0.5 rooms
    # this feels way to complicated
    scaled_zimmeranzahl_r_o <- unique_combinations[
      ,
      (as.numeric(unique_combinations[i, "zimmeranzahl"]) + zimmeranzahl_r_o) / as.numeric(unique_combinations[i, "zimmeranzahl"]) - 1
    ]
    # scaling around 0 causes div by 0 and inf for everything else
    # just increase etage by one? other solution?
    data_to_similarity <- scale(unique_combinations, center = F, scale = unique_combinations[i]) |> as.data.table()
    
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
  diag(similarity_index_list) = 0
  
  setnames(similarity_index_list, as.character(first_occurence_ids))
  setnames(similarity_dist_list, as.character(first_occurence_ids))
  
  # setup and run the actual clustering
  clustering <- cluster$new(
    # since apply performs action on rows while all other functions use columns
    cluster_options = similarity_index_list,
    #*similarity_index_list
    distance = similarity_dist_list,
    means = rowMeans(similarity_index_list * similarity_dist_list, na.rm = T)
  )
  clustering$determine_cluster_centers()
  
  if (anyDuplicated(clustering$centers$counting_id)) {
    
    # filter/fix duplicates within $centers here if they exist
    # currently its always being parents > being a child to ease calc
    # otherwise there has to be a cost assigned for non-parenthood
    
    clustering$centers <- clustering$centers[
      ,
      .SD[which.min(sim_dist)],
      by = "counting_id"
    ]
  }
} else {
  
  # this could be function since im doing it more than once
  # does this make the if within cluster-class irrelevant?
  clustering <- cluster$new(
    cluster_options = NULL
  )
  clustering$centers = data.table(
    "counting_id" = as.numeric(first_occurence_ids),
    "parent" = as.numeric(first_occurence_ids),
    "sim_dist" = 0,
    "sim_index" = 0
  )
  
}
  
  # Unit-Test ---------------------------------------------------------------
  
  # make sure every combination got clustered
  tar_assert_true(nrow(clustering$centers) == nrow(unique_combinations))
  
  
  # merge cluster results to inital data and return
  out <- geo_grouped_data[
    clustering$centers,
    on = .(first_occurence_ids = counting_id),
    allow.cartesian = T
  ]
  
  # make sure we dont output more obs than we input
  tar_assert_true(nrow(geo_grouped_data) == nrow(out))
  
  # check if no NAs were created somewhere
  #tar_assert_true(!out[,anyNA(.SD), .SDcols = c("sim_index","sim_dist","parent")])
  
  return(out)
}

