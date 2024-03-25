similarity_cost_function <- function(clustering_centers) {
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

  # NOTE: to save on variables all competitors/winners could be integrated into a
  # piping structure. decided against it to make it easier to see what
  # the individual steps are doing, since it gets quite complicated during
  # the gains calculation

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
  unique_clustering_centers <- unique_clustering_centers[
    !is.na(sim_index)
  ]

  # competitions ------------------------------------------------------------
  # isolate cases where conflicts arose
  competing_parents <- unique_clustering_centers[
    ,
    .SD[.N >= 2],
    by = "counting_id"
  ]

  # check if cleaning up NAs fixed the problem already
  if (nrow(competing_parents) != 0) {
    # parent vs child competing -----------------------------------------------
    # find parents which have conflicting classifications
    parent_children_ids <- competing_parents[
      parent == counting_id
    ]$parent

    ## first option: x parent of cluster x with children y_n -> gain = sum(similiarity of children)
    # isolate competitors based on ids
    parent_children_competitors <- unique_clustering_centers[
      .(parent_children_ids),
      on = "parent"
    ]

    # calculate gains of being x being a parent
    parent_gains <- parent_children_competitors[
      ,
      .(
        "cluster_sim_dist" = fifelse(
          .N > 1,
          sum(sim_dist) / (.N - 1), # average excluding parent itself
          1 # just the parent itself, undesirable
        )
      ),
      by = "parent",
    ]

    ## second option: x child of cluster z -> gain = similarity to parent
    # isolate competitors based on ids
    child_parent_competitors <- unique_clustering_centers[
      .(parent_children_ids),
      on = "counting_id"
    ]
    # this kinda does nothing anymore? refactor this
    child_gains <- child_parent_competitors[
      counting_id != parent,
      .("single_sim_dist" = mean(sim_dist)),
      by = "counting_id"
    ]


    tar_assert_true(nrow(child_gains) == nrow(parent_gains), msg = glue::glue("Different number of gains:{parent_gains$parent}"))

    # Compare gains and choose lower sim_dist (more similiarity gained)
    # maybe remove best gains and apply recursively?
    gains_comparison <- parent_gains[
      child_gains,
      .(
        "counting_id" = counting_id,
        "winner_type" = fifelse(
          cluster_sim_dist >= single_sim_dist, "child", "parent"
        )
      ),
      on = .(parent == counting_id)
    ]
    # split gain-winners into parent-/child type since they cause each
    # have different consequences
    winner_ids <- split(gains_comparison, by = "winner_type", keep.by = F)

    # Unit-Test
    # Check if any ids have been assigned as both parent and child winner
    tar_assert_true(
      !any(winner_ids$child %in% winner_ids$parent),
      msg = "Overlapping winners found! {winner_ids$child %in% winner_ids$parent}"
    )
    # Edge case -> no parents chosen, default to best parent
    if (length(winner_ids$parent) == 0) {
      winner_ids$parent <- parent_gains[,.SD[which.min(cluster_sim_dist)], .SDcols = "parent"] |> setnames("counting_id")
    }
    
    if(length(winner_ids) == 2){
      
      unique_clustering_centers = child_removal_f(parent_gains, winner_ids, unique_clustering_centers)
      
      if(anyDuplicated(unique_clustering_centers$counting_id) != 0){
        parent_removal <- competing_parents[
          .(winner_ids$child),
          on = .(parent = counting_id)
        ][parent == counting_id]
        
        # # dont remove what was already dealt with above
        # parent_removal = parent_removal[!child_removal, on = "parent"]
        
        unique_clustering_centers <- unique_clustering_centers[
          !parent_removal,
          on = .(parent)
        ]
      }
      
      
    } else {
      unique_clustering_centers = child_removal_f(parent_gains, winner_ids, unique_clustering_centers)
      if(anyDuplicated(unique_clustering_centers$counting_id) != 0){
        unique_clustering_centers = unique_clustering_centers[, .SD[which.min(sim_dist)], by = "counting_id"]
      }
    }
    # this some magic -> if it works refactor
    missing_ids = clustering_centers[!unique_clustering_centers, on = "counting_id"] |> na.omit() |> unique()
    if(nrow(missing_ids) != 0 ){
      alternative_parent = missing_ids[, .SD[which.min(sim_dist)], by = "counting_id"]
      if(any(alternative_parent$parent %in% unique_clustering_centers$parent)){
        unique_clustering_centers = rbind(
          unique_clustering_centers, 
          alternative_parent
          )
      } else {
        unique_clustering_centers = rbind(unique_clustering_centers, alternative_parent)
      }
      
      
      
    }
  }

  # Unit-test
  id_check <- unique(clustering_centers$counting_id) %in% unique_clustering_centers$counting_id
  tar_assert_true(all(id_check), msg = glue::glue("missings ids:{unique(clustering_centers$counting_id)[!id_check]}"))
  tar_assert_true(anyDuplicated(unique_clustering_centers$counting_id) == 0, msg = glue::glue("Duplicates still found:{unique_clustering_centers$counting_id}"))
  

  return(unique_clustering_centers)
}
