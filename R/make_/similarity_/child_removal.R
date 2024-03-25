child_removal_f = function(parent_gains, winner_ids,unique_clustering_centers){
  
  competing_parents <- unique_clustering_centers[
    ,
    .SD[.N >= 2],
    by = "counting_id"
  ]
  
  # all parent-type winners eliminate them being a child of someone else
  child_removal <- competing_parents[
    .(winner_ids$parent),
    on = .(counting_id)
  ][!parent == counting_id]# & parent %in% counting_id
  
  # determine if these choices are exclusive -> select one with best sim_dist average
  mutual_removal_ids <- child_removal[parent %in% counting_id, .(parent)]
  if(nrow(mutual_removal_ids != 0)){
    # should always be multiple of 2
    tar_assert_true(nrow(mutual_removal_ids) %% 2 == 0, msg = glue::glue("removal_ids not multiple of two {mutual_removal_ids$parent}"))
    
    # this is probably too complicated, but does its job
    # make temp id which groups two consecutive items
    mutual_removal_pairs <- parent_gains[mutual_removal_ids, on = "parent"][
      ,
      temp_id := fifelse(
        .I %% 2 == 0,
        .I - 1,
        .I
      )
    ]
    ids_to_keep <- mutual_removal_pairs[, .SD[which.min(cluster_sim_dist)], by = temp_id][, .(parent)] |> unique()
    
    
    # anti-join to only keep what should be dropped
    child_removal <- child_removal[!mutual_removal_ids[ids_to_keep, on = "parent"], on = "parent"]
    
  }
  
  # check if this merge did what its intended to do
  tar_assert_true(all(child_removal[counting_id %in% winner_ids$parent, counting_id != parent]), msg = "Child removal failed")
  
  # when is this necessary?
  if(uniqueN(child_removal$parent) == 1){
    child_removal <- competing_parents[
      !child_removal,
      on = .(parent)
    ]
    
    unique_clustering_centers = unique_clustering_centers[
      !child_removal,
      on = .(counting_id,parent)
    ]
  } else {
    unique_clustering_centers = unique_clustering_centers[
      !child_removal,
      on = .(parent)
    ]
  }
  
  # check if this solved the issue
  competing_parents <- unique_clustering_centers[
    ,
    .SD[.N >= 2],
    by = "counting_id"
  ]
  
  if(nrow(competing_parents) != 0 & nrow(child_removal) > 1){
    unique_clustering_centers = child_removal_f(parent_gains, winner_ids, unique_clustering_centers)
  } 
  return(unique_clustering_centers)
  
}

