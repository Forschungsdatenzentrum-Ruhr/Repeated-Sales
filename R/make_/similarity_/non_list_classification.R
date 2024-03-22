non_list_classification <- function(parent_grouped_data, data_end_date) {
  # WK data !!
  # latlon_utm == 5914585.51138591603185.2001455
  # contains long update chain + miss example

  # this is fairly ugly, but cant reference non_list_duration in second mutation
  # due to data.table not finding it
  # throws error without copy due to using := within .SD
  parent_grouped_data <- copy(parent_grouped_data)
  setkey(parent_grouped_data, amonths, emonths)

  parent_grouped_data_non_list <- parent_grouped_data[
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
    # within parent grouped frame figure out if listings has been made
    # in within same year/month
    # these are currently problematic since classification missclassifies
    same_time_listing := .N >= 2,
    by = "amonths"
  ][, rs_id := first(counting_id)]

  ## this is probably too big of a step as it changes the original data -> leave this to reasearchers
  # set index to allow for binary operations here and later
  # setindex(parent_grouped_data_non_list, non_list_reason)

  # consider catching cluster that have no sales or are single observations (should only happen at the very recent end of the data)
  # these are dropped anyway but look confusing since they are NA
  # first_sold_id = parent_grouped_data_non_list["Sold", first(counting_id), on = "non_list_reason", with = T]
  
  # parent_grouped_data_connected <- parent_grouped_data_non_list[
  #   !"Miss",
  #   c("start_position", "end_position") := which_range(non_list_reason),
  #   on = "non_list_reason"
  # ][,
  #   ":="(
  #   # can calculate something like price difference from initial offering to actual sale price
  #   amonths = fifelse(
  #     !(start_position == end_position) & !is.na(start_position),
  #     amonths[start_position],
  #     amonths
  #   ),
  #   start_position = NULL,
  #   end_position = NULL,
  #   rs_id = first_sold_id
  #   )
  #   # if missed due to same_time_listing reset rs_id
  # ][non_list_reason == "Miss" & same_time_listing == TRUE, rs_id := counting_id]

  # # check if all ids of parents are ids that were actually sold, non update/miss-classified
  # # since i reset rs_ids above, this no longer works
  # tar_assert_true(
  #   parent_grouped_data_connected[counting_id == rs_id & non_list_reason != "Sold", .N] == 0
  # )

  # check if any mistakes were made (starting date is before end date)
  tar_assert_true(
    !any(parent_grouped_data_non_list[, amonths > emonths]),
    msg = glue::glue("amonths > emonths for {unique(parent_grouped_data_non_list$parent)} at {unique(parent_grouped_data_non_list$latlon_utm)}")
  )

  # check if no NAs were created somewhere
  tar_assert_true(
    !parent_grouped_data_non_list[, anyNA(.SD), .SDcols = c("non_list_reason", "non_list_duration")],
    msg = glue::glue("NAs created for {unique(parent_grouped_data_non_list$latlon_utm)}")
  )

  return(parent_grouped_data_non_list)
}
