child_removal_f <- function(parent_gains, winner_ids, unique_clustering_centers) {
  #' @title Child removal function
  #' 
  #' @description This function removes children from the unique_clustering_centers data.table
  #' @param parent_gains data.table. Data set with parent gains
  #' @param winner_ids data.table. Data set with winner ids
  #' @param unique_clustering_centers data.table. Data set with unique clustering centers
  #' 
  #' @return data.table. Data set with removed children
  #' @author Thorben Wiebe
  # ------------------------------------------------
  # Input validation
  input_check(parent_gains, "data.table")
  input_check(winner_ids, "data.table")
  input_check(unique_clustering_centers, "data.table")
  # ------------------------------------------------
  # identify competing parents
  competing_parents <- unique_clustering_centers[
    ,
    .SD[.N >= 2],
    by = "counting_id"
  ]
  # all parent-type winners eliminate them being a child of someone else
  child_removal <- competing_parents[
    .(winner_ids$parent),
    on = .(counting_id)
  ][!parent == counting_id]

  # determine if these choices are exclusive -> select one with best sim_dist average
  mutual_removal_ids <- child_removal[parent %in% counting_id][counting_id %in% parent]

  if (nrow(mutual_removal_ids) > 1) {
    # this is probably too complicated, but does its job
    mutual_removal_pairs <- mutual_removal_ids[
      mutual_removal_ids[, .(parent, counting_id)],
      pair_row_number := .I,
      on = .(parent == counting_id, counting_id == parent)
    ] |> na.omit()
    # Unit test: removal_ids should always be multiple of 2
    tar_assert_true(nrow(mutual_removal_pairs) %% 2 == 0, msg = glue::glue("removal_ids not multiple of two {mutual_removal_pairs$parent}"))

    for (i in seq_along(nrow(mutual_removal_pairs))) {
      # extract the pair
      pair_row_number_vector <- c(i, mutual_removal_pairs |> slice(i) |> select("pair_row_number") |> pull()) |> sort()
      # combine pair into temp id and assign
      mutual_removal_pairs[pair_row_number %in% pair_row_number_vector, temp_id := paste0(pair_row_number_vector, collapse = "_")]
    }
    # add gains to pairs to make decision
    mutual_removal_pairs <- parent_gains[mutual_removal_pairs, on = "parent"]
    ids_to_keep <- mutual_removal_pairs[, .SD[which.min(cluster_sim_dist)], by = temp_id][, .(parent)] |> unique()

    # anti-join to only keep what should be dropped
    child_removal <- child_removal[!mutual_removal_ids[ids_to_keep, on = "parent"], on = "parent"]
  }

  # check if this removal solved the issue
  tar_assert_true(all(child_removal[counting_id %in% winner_ids$parent, counting_id != parent]), msg = "Child removal failed")

  # remove children from unique_clustering_centers
  if (uniqueN(child_removal$parent) == 1) {
    # edge case: only one parent to remove -> remove parent by counting_id and parent
    child_removal <- competing_parents[
      !child_removal,
      on = .(parent)
    ]
    unique_clustering_centers <- unique_clustering_centers[
      !child_removal,
      on = .(counting_id, parent)
    ]
  } else {
    # default case: remove by parent
    unique_clustering_centers <- unique_clustering_centers[
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
  # edge case: if there are still competing parents, recurse
  if (nrow(competing_parents) != 0 & nrow(child_removal) > 1) {
    unique_clustering_centers <- child_removal_f(parent_gains, winner_ids, unique_clustering_centers)
  }
  # ------------------------------------------------
  # Unit test
  empty_check(unique_clustering_centers)
  # ------------------------------------------------
  return(unique_clustering_centers)
}
