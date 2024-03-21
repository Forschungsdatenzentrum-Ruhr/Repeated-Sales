prepare_split <- function(single_index, single_index_name, grouping = c("date_quarter", "gid2019")) {
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
    # # remove stuttgard
    # single_index = single_index[gid2019 != 08111000]
  }
  # "gid2019"
  
  single_index <- single_index[, .(mean_index = mean(na.omit(index), na.rm = T)), by = grouping][, index_type := single_index_name]
  
  return(single_index)
}