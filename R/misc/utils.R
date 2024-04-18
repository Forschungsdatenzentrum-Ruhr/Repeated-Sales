### Utility functions
# ----------------------------------------------
## Calcuation helper functions
# helps connect selling point to first preceding update
# update chain upto and including selling point are connected into one listing
which_range <- function(non_list_reason_vec) {
  # full NA vectors to return object of same length
  returner_end <- returner_start <- rep(NA, length(non_list_reason_vec))

  # id end of update chains i.e. when object was sold
  # update chains can length 0 if object is sold without being updated
  end_position <- which(non_list_reason_vec == "Sold")

  # find subsequent end of update chain
  start_position <- shift(end_position + 1, fill = 1, type = "shift")

  returner_start[end_position] <- start_position
  returner_end[end_position] <- end_position

  return(list(returner_start, returner_end))
}


between_helper <- function(i_value, j_value, offset) {
  abs_diff = abs(i_value - j_value)
  out <- (abs_diff < offset) | sapply(abs_diff, function(x){isTRUE(all.equal(x,offset))})
  return(out)
}
# ----------------------------------------------
## Unit test helper functions
empty_check = function(.data) {
  # check if data is empty
  tar_assert_true(
    all(!dim(data) %in% c(0, NULL)),
    glue::glue("{deparse(substitute(.data))} is empty.")
  )
  return(NULL)
}

input_check = function(.data,.class){
  # check if data is of correct class
  tar_assert_true(
    inherits(.data, .class),
    glue::glue("{deparse(substitute(.data))} must be a {.class}.")
  )
  return(NULL)
}

# ----------------------------------------------
## Progress bar helper functions
custom_progress_bar <- function(classification_type, .GRP, .GRPN, mod = 1000) {
  # the .envir argument causes the progress_bar used to be the global one
  if (.GRP == 1) {
    start_time <<- Sys.time()
    # Initialization of progress bar and some stats
    cli::cli_alert_info(glue::glue("Starting {classification_type} at {start_time}"))
    cli::cli_alert_info(glue::glue("Total groups: {.GRPN}"))
    cli::cli_progress_bar("  Classifying ...", total = (.GRPN / mod), .envir = parent.frame(n = sys.nframe()))
  } else if (.GRP == .GRPN) {
    # Finish and cleanup
    cli::cli_alert_success(glue::glue("Finished {classification_type} after {format(Sys.time() - start_time, format = '%H:%M')}"))
    cli::cli_progress_done(.envir = parent.frame(n = sys.nframe()))
  } else if (!(.GRP %% mod)) {
    # Update everytime mod times x is hit
    cli::cli_progress_update(.envir = parent.frame(n = sys.nframe()))
  }

  return(NULL)
}


# # Remove?
# check_nonsensical_listings <- function(data_connected, data_name) {
#   # calc ayear/eyear from amonths/emonths
#   data_connected <- copy(data_connected)[, ":="(ayear = amonths %/% 12, eyear = emonths %/% 12)]

#   # table ayear vs eyear and remove name column
#   ayear_eyear_table <- data_connected |>
#     tabyl(ayear, eyear) |>
#     select(-any_of(c("ayear", "eyear"))) |>
#     as.matrix()

#   # save non-modified table for saving, next step modifies in place
#   table_to_save <- ayear_eyear_table

#   # set upper triangle of matrix including diag as NA (these are okay to be >0)
#   ayear_eyear_table[upper.tri(ayear_eyear_table, diag = T)] <- NA

#   # calc sum of colsums to check that entire lower triangle of matrix is contains only 0
#   if (!sum(colSums(ayear_eyear_table, na.rm = T)) == 0) {
#     # save to file
#     table_to_save |>
#       htmlTable(rnames = F) |>
#       kableExtra::save_kable(paste0(output_path, "/", data_name, "_", "ayear", "_", "eyear", ".png"))

#     # there should be a better way to stop the pipeline with msg but i wasnt able to find one yet
#     tar_assert_true(
#       FALSE,
#       msg = glue::glue("nonsensical ayear/eyear combinations found at: {unique(data_connected$latlon_utm)}")
#     )

#     return(NULL)
#   }
# }