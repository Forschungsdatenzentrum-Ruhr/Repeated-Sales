prepare_combined <- function(single_index, single_index_name, grouping) {
  #' @title Prepare combined
  #' 
  #' @description This function prepares the indices for the split.
  #' @param single_index data.table. Single index data.
  #' @param single_index_name character. Name of the single index.
  #' @param grouping character. Grouping variable.
  #' 
  #' @return data.table. Prepared indices for the split.
  #' @author Thorben Wiebe
  # ----------------------------------------------
  # Input validation
  input_check(single_index, "data.table")
  input_check(single_index_name, "character")
  input_check(grouping, "character")
  # ----------------------------------------------
  # add date_quarter column if not present
  if (!("date_quarter" %in% names(single_index))) {
    single_index <- make_date_quarter(single_index)
  } else {
    # repeated index calculation combines the date_quarter and gid2019 columns
    # resplit them here
    gid_date <- single_index[["date_quarter"]] |> tstrsplit("\\.")
    single_index <- single_index[, ":="(
      gid2019 = gid_date[[1]],
      date_quarter = gid_date[[2]] |> as.Date("%Y-%m-%d"),
      index = as.numeric(index)
    )]

  }
  # mean index calculation
  single_index <- single_index[, .(mean_index = mean(na.omit(index), na.rm = T)), by = grouping][, index_type := single_index_name]
  # ----------------------------------------------
  # Unit test
  empty_check(single_index)
  # ----------------------------------------------
  return(single_index)
}