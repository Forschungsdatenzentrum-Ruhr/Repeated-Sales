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
  # each observation can only be a parent or child, never both
  # isolate cases where conflicts arose
  # to dissolve this conflict compare similiarity gains from each case

  # unique deals with a parent being chosen as such from multiple listings
  # since the parent is the same either way, no special consideration is necessary
  # TODO: consider dropping NAs in sim_index here aswell since these should never be chosen
  competing_parents <- unique(clustering_centers)[, .SD[.N >= 2], by = "counting_id"]

  # parent vs parent competing:
  # x is potential child of y or z but not itself -> choose lowest sim_dist
  # this also contains all listings with are their own parent -> used in first option
  parent_parent_competitors <- competing_parents[, .SD[which.min(sim_dist)], by = "counting_id"]

  # isolate cases where there was pure parent vs parent competition
  # this likely influences parent vs children competiton and should be remerged to initial data
  parent_parent_winners <- parent_parent_competitors[parent != counting_id]


  ## parent vs child competing:
  # first option: x parent of cluster x with children y_n -> gain = sum(similiarity of children)
  parent_children_competitors <- parent_parent_competitors[parent == counting_id]$parent

  parent_gains <- clustering_centers[
    .(parent_children_competitors),
    on = "parent"
  ][,
    .("cluster_sim_dist" = sum(sim_dist)),
    by = "parent",
  ]

  # second option: x child of cluster z -> gain = similarity to parent
  # another way to think about it: ^ is lost if the other option is chosen
  child_gains = clustering_centers[
    .(parent_children_competitors),
    on = "counting_id"
  ][
    !.(parent_children_competitors),
    .("counting_id" = counting_id,"single_sim_dist" = sim_dist),
    on = "parent"
  ]

  gains_comparison = parent_gains[
    child_gains,
    "winner" := fifelse(
      cluster_sim_dist >= single_sim_dist, "parent", "child"
    ),
    on = .(parent == counting_id)
  ]
}
