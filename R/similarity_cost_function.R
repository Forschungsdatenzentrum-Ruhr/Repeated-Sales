similarity_cost_function <- function(clustering_centers = NA) {
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

  # geo_grouped_data = RED |> filter(latlon_utm == "5914872.28545209603584.936611244")


  # Explanation + Prep ------------------------------------------------------

  # each observation can only be a parent or child, never both
  # to dissolve this conflict compare similarity gains from each case
  # another way to think about it: similarity is lost if the other option is chosen
  
  # unique deals with a parent being chosen as such from multiple cluster
  # since the parent is the same either way, no special consideration is necessary
  unique_clustering_centers <- unique(clustering_centers)


  # clean_up of NAs ---------------------------------------------------------

  # deal with NAs, these should always have at least themselves as well as
  # another potential parent -> choose minimal sim_dist that isnt NA
  # for full explanation of why/how this occurs refer to cluster_class.R

  # find wrongfully chosen counting_ids
  na_parent_ids <- unique_clustering_centers[
    is.na(sim_index)
  ]$counting_id
  
  # only proceed if any wrongfully chosen counting_ids are found
  if (length(na_parent_ids) != 0) {
    
    # isolate competitors based on ids
    na_parent_competitors <- unique_clustering_centers[
      .(na_parent_ids),
      on = "counting_id"
    ]
    
    # compare competitors and chose winners
    na_parent_winners <- na_parent_competitors[
      , .SD[which.min(sim_dist)],
      by = "counting_id"
    ]
    
    # keep only non-na_parent_winners for further considerations
    unique_clustering_centers <- unique(
      rbindlist(
        rlang::list2(
          unique_clustering_centers[
            !is.na(sim_index)
          ],
          na_parent_winners
        )
      )
    )
  }
  

  # post na competition -----------------------------------------------------

  # isolate cases where conflicts arose
  competing_parents <- unique_clustering_centers[
    , .SD[.N >= 2],
    by = "counting_id"
  ]
  # check if cleaning up Nas fixed the problem already
  if(length(competing_parents) != 0){
    
    # parent vs parent competing ----------------------------------------------
    
    # x is potential child of y or z but not itself -> choose lowest sim_dist
    # this also contains all listings with are their own parent -> used in first option
    parent_parent_competitors <- competing_parents[
      , .SD[which.min(sim_dist)],
      by = "counting_id"
    ]
    
    # isolate cases where there was pure parent vs parent competition
    # this likely influences parent vs children competition and should be remerged to initial data
    parent_parent_winners <- parent_parent_competitors[
      parent != counting_id
    ]
    
    
    # parent vs child competing -----------------------------------------------
    
    # find parents which have conflicting classifications
    parent_children_ids <- parent_parent_competitors[
      parent == counting_id
    ]$parent
    
    if (length(parent_children_ids) != 0) {
      
      ## first option: x parent of cluster x with children y_n -> gain = sum(similiarity of children)
      # isolate competitors based on ids
      parent_children_competitors <- unique_clustering_centers[
        .(parent_children_ids),
        on = "parent"
      ]
      
      # calculate gains of being x being a parent
      parent_gains <- parent_children_competitors[
        ,
        .("cluster_sim_dist" = sum(sim_dist)),
        by = "parent",
      ]
      
      ## second option: x child of cluster z -> gain = similarity to parent
      # isolate competitors based on ids
      child_parent_competitors <- unique_clustering_centers[
        .(parent_children_ids),
        on = "counting_id"
      ]
      
      child_gains <- child_parent_competitors[
        !.(parent_children_ids),
        .("counting_id" = counting_id, "single_sim_dist" = sim_dist),
        on = "parent"
      ]
      
      # Compare gains
      gains_comparison <- parent_gains[
        child_gains,
        "winner" := fifelse(
          cluster_sim_dist >= single_sim_dist, "parent", "child"
        ),
        on = .(parent == counting_id)
      ]
    }
  }
 
}
