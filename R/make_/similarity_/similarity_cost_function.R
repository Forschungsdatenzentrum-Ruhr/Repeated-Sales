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

  # Curr Error
  #coords = RED |> filter(counting_id == "5344") |> pull(latlon_utm)
  #geo_grouped_data = RED |> filter(latlon_utm == coords & balkon == 1)
  
  # NA-Example
  # geo_grouped_data = RED |> filter(latlon_utm == "5914872.28545209603584.936611244")

  # parent_child Example
  # geo_grouped_data = RED |> filter(latlon_utm == "5915033.15972125602782.786688613")

  # NOTE: to save on variables all competitors/winners could be integrated into a
  # piping structure. decided against it to make it easier to see what
  # the individual steps are doing, since it gets quite complicated during
  # the gains calculation

  # Explanation + Prep ------------------------------------------------------
  
  # empty data.table to appended to
  new_namesmes(clustering_centers)
  final_removal = setNames(data.table(matrix(nrow = 0, ncol = length(new_names))), new_names)
  
  # each observation can only be a parent or child, never both
  # to dissolve this conflict compare similarity gains from each case
  # another way to think about it: similarity is lost if the other option is chosen

  # unique deals with a parent being chosen as such from multiple cluster
  # since the parent is the same either way, no special consideration is necessary
  unique_clustering_centers <- unique(clustering_centers)

  # clean_up of NAs ---------------------------------------------------------

  # deal with NAs, these should always have at least themselves as well as
  # another potential parent -> choose minimal sim_dist that isnt NA
  # NOTE: check git if simple solution isnt working - just dropping NAs should
  # work out fine, since competing parents are sorted out later anyway
  # for full explanation of why/how this occurs refer to cluster_class.R

  # keep only non-NAs for further considerations
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
  
  # this check likely needs to be more complicated since not every parent_child
  # competion has to pass through parent_parent competition
  # check if cleaning up NAs fixed the problem already
  if (length(competing_parents) != 0) {
  #if(anyDuplicated(competing_parents$parent) != 0)
    # parent vs parent competing ----------------------------------------------

    # x is potential child of y or z but not itself -> choose lowest sim_dist
    # this also contains all listings with are their own parent -> used in first option
    parent_parent_winners <- competing_parents[
      parent != counting_id,
      .SD[which.min(sim_dist)],
      by = "counting_id"
    ]

    # isolate losers that arent parents of themselves
    parent_parent_loser <- competing_parents[
      !parent_parent_winners,
      on = .(counting_id, parent)
    ][parent != counting_id]

    # remove non-winning classifications from cluster via anti-join
    unique_clustering_centers <- unique_clustering_centers[
      !parent_parent_loser,
      on = .(counting_id, parent)
    ]

    # recheck if conflicts still exist
    competing_parents <- unique_clustering_centers[
      ,
      .SD[.N >= 2],
      by = "counting_id"
    ]

    if (length(competing_parents) != 0) {
      # parent vs child competing -----------------------------------------------
      # find parents which have conflicting classifications
      parent_children_ids <- competing_parents[
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
          .("cluster_sim_dist" = sum(sim_dist)), #mean(sim_dist)),
          by = "parent",
        ]

        ## second option: x child of cluster z -> gain = similarity to parent
        # isolate competitors based on ids
        child_parent_competitors <- unique_clustering_centers[
          .(parent_children_ids),
          on = "counting_id"
        ]

        child_gains <- child_parent_competitors[
          counting_id != parent,
          .("counting_id" = counting_id, "single_sim_dist" = sim_dist),
        ]
        
        
        tar_assert_true(nrow(child_gains) == nrow(parent_gains))

        # Compare gains and choose lower sim_dist (more similiarity gained)
        # this can have conflicting outcomes -> ISSUE
        # children chose each other as their parent -> stronger rule?
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
          msg = winner_ids
        )

        # apply winner selection to initial cluster_centers by anti-joining
        # the opposite result
        if (length(winner_ids$parent) != 0) {
          # all parent-type winners eliminate them being a child of someone else
          # NOTE This should be possible in one join but i cant find a way to
          # add the latter select into the on statement
          child_removal <- competing_parents[
            .(winner_ids$parent),
            on = .(counting_id = counting_id)
          ][!parent == counting_id]
        
          
          # check if this merge did what its intended to do
          tar_assert_true(all(child_removal[counting_id %in% winner_ids$parent, counting_id != parent]))
          
          final_removal = rbindlist(list(final_removal, child_removal))
        }
        if (length(winner_ids$child) != 0) {
          # all child-type winners eliminate them being a parent
          parent_removal <- competing_parents[
            .(winner_ids$child),
            on = .(parent = counting_id)
          ][parent == counting_id]
          
          # check if this merge did what its intended to do
          tar_assert_true(all(parent_removal[counting_id %in% winner_ids$child, counting_id == parent]))
          
          
          final_removal = rbindlist(list(final_removal, parent_removal))
        }
        # combine removals which each being possibly empty

        # after elimination
        unique_clustering_centers <- unique_clustering_centers[
          !final_removal,
          on = .(counting_id, parent)
        ]
      }
    }
  }
  
  #Unit-test
  tar_assert_true(anyDuplicated(unique_clustering_centers) == 0)

  
  
  # tar_assert_true(all(clustering_centers$counting_id %in% unique_clustering_centers$counting_id))
  # tar_assert_true(all(unique_clustering_centers$counting_id %in% clustering_centers$counting_id))
  
  
  return(unique_clustering_centers)
}
