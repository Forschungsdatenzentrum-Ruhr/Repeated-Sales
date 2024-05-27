write_total_point_change <- function(start_values, end_values, data_type, split_bool = TRUE) {
  # ----------------------------------------------
  # Input validation
  input_check(start_values, "data.table")
  input_check(end_values, "data.table")
  # ----------------------------------------------
  if(split_bool){
    by_vec = c("index_type", "gid_names")
  } else{
    by_vec = "index_type"
  }
  prefix = fifelse(split_bool, "split", "combined")

  # write total points changes to file
  total_change <- merge(start_values, end_values, by = by_vec, suffixes = c("_start", "_end"))
  # round and calculate total change
  total_change[, ":="(
    based_index_start = round(based_index_start, 2),
    based_index_end = round(based_index_end, 2),
    total_change = round((based_index_end - based_index_start) / based_index_start * 100, 2)
  )]
  fwrite(total_change, glue::glue("output/{data_type}/{data_type}_{prefix}_total_change.txt"))

  invisible(total_change)
}
