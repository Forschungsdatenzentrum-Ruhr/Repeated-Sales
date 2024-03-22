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

  # empty data.table to appended to
  new_names <- names(clustering_centers)
  final_removal <- setNames(data.table(matrix(nrow = 0, ncol = length(new_names))), new_names)

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
        "cluster_sim_dist" = sum(sim_dist) / (.N - 1) # average excluding parent itself
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
      .("single_sim_dist" = min(sim_dist)),
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
    # Edge case -> no parents chosen, happens when everything is within decimals
    if (length(winner_ids$parent) == 0) {
      winner_ids$parent <- parent_gains[,.SD[which.min(cluster_sim_dist)], .SDcols = "parent"] |> setnames("counting_id")
    }

    # all parent-type winners eliminate them being a child of someone else
    child_removal <- competing_parents[
      .(winner_ids$parent),
      on = .(counting_id = counting_id)
    ][!parent == counting_id]

    # determine if these choices are exclusive -> select one with best sim_dist average

    mutual_removal_ids <- child_removal[counting_id %in% parent, .(parent)]
    # should always be multiple of 2
    tar_assert_true(nrow(mutual_removal_ids) %% 2 == 0, msg = "removal_ids not multiple of two")

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
    ids_to_keep <- mutual_removal_pairs[, .SD[which.min(cluster_sim_dist)], by = temp_id][, .(parent)]

    # anti-join to only keep what should be dropped
    child_removal <- child_removal[!mutual_removal_ids[ids_to_keep, on = "parent"], on = "parent"]

    # check if this merge did what its intended to do
    tar_assert_true(all(child_removal[counting_id %in% winner_ids$parent, counting_id != parent]), msg = "Child removal failed")

    unique_clustering_centers <- unique_clustering_centers[
      !child_removal,
      on = .(parent)
    ]
  }

  # Unit-test
  tar_assert_true(anyDuplicated(unique_clustering_centers$counting_id) == 0, msg = glue::glue("Duplicates still found:{unique_clustering_centers$counting_id}"))
  id_check <- unique(clustering_centers$counting_id) %in% unique_clustering_centers$counting_id
  tar_assert_true(all(id_check), msg = glue::glue("missings ids:{unique(clustering_centers$counting_id)[!id_check]}"))

  return(unique_clustering_centers)
}
