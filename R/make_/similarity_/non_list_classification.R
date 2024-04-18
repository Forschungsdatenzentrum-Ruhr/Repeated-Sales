non_list_classification <- function(parent_grouped_data, data_end_date) {
  #' @title Classify listings based on non-listing duration
  #' 
  #' @description This function classifies listings based on their non-listing duration
  #' @param parent_grouped_data data.table. Data set with grouped data
  #' @param data_end_date numeric. End date of data
  #' 
  #' @return data.table. Data set with classified listings
  #' @author Thorben Wiebe
  # ------------------------------------------------
  # Input validation
  input_check(parent_grouped_data, "data.table")
  input_check(data_end_date, "numeric")
  # ------------------------------------------------
  # make copy to modifiy keys
  parent_grouped_data <- copy(parent_grouped_data)
  # key data.table to allow for binary operations
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
    # subsequent one which actually sold the listing
    non_list_reason := .(fcase(
      non_list_duration < 0, "Miss",
      non_list_duration >= 0 & non_list_duration <= time_offset, "Update",
      non_list_duration > time_offset, "Sold"
    ))
  ][,
    # within parent grouped frame figure out if listings has been made
    # in within same year/month
    same_time_listing := .N >= 2,
    by = "amonths"
    # assign new id to each parent group which is the first id of the group rather than the actual centroid/parent chosen
  ][, rs_id := first(counting_id)]

  # ------------------------------------------------
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
  # ------------------------------------------------
  # Unit test
  empty_check(parent_grouped_data_non_list)
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
  #------------------------------------------------
  return(parent_grouped_data_non_list)
}
