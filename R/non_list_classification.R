non_list_classification <- function(parent_grouped_data = NA, data_end_date = NA) {
  # WK data !!
  # latlon_utm == 5914585.51138591603185.2001455
  # contains long update chain + miss example

  # this is fairly ugly, but cant reference non_list_duration in second mutation 
  # due to data.table not finding it
  # throws error without copy due to using := within .SD
  parent_grouped_data_non_list <- copy(parent_grouped_data)[
    ,
    non_list_duration := fifelse(
      is.na(shift(amonths, 1, type = "lead") - emonths),
      data_end_date - emonths,
      shift(amonths, 1, type = "lead") - emonths
    )
  ][
    ,
    # update wording might be misleading since the actual update is the 
    # subsequent one which actually sold
    # updated/overwrite/super-ceded?
    non_list_reason := .(fcase(
      non_list_duration < 0, "Miss",
      non_list_duration >= 0 & non_list_duration <= time_offset, "Update",
      non_list_duration > time_offset, "Sold"
    ))
  ][,
    same_time_listing:= .N >= 2,
    by = c("amonths")
    
  ]
  
  # set index to allow for binary operations here and later
  setindex(parent_grouped_data_non_list, non_list_reason)
  
  parent_grouped_data_connected <- parent_grouped_data_non_list[
    !"Miss",
    c("start_position", "end_position") := which_range(non_list_reason),
    on = "non_list_reason"
  ][
    ,
    # can calculate something like price difference from initial offering to actual sale price
    amonths := fifelse(
      !start_position == end_position & !is.na(start_position),
      amonths[start_position],
      amonths
    )
  ]
  parent_grouped_data_connected[, c("start_position","end_position") := NULL]
  
  # check if no NAs were created somewhere
  tar_assert_true(!parent_grouped_data_connected[,anyNA(.SD), .SDcols = c("non_list_reason","non_list_duration")])

  return(parent_grouped_data_connected)
}
