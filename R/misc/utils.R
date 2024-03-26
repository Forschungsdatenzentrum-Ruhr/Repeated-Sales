#' Helper and Debug Functions, WIP-Section
#'
#' @description
#' Universal functions used during different steps of pipeline aswell as WIP-
#' Section and debugging
#'

output_path_json <- function(output_path) {
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = T)
  }

  write_json(
    exportJSON,
    paste0(output_path, "/", "settings_used.json")
  )
}


filter_unique_options <- function(unique_options, new_names, use_which = T) {
  # error in this function which causes overflow in data.table???
  if (!is.null(unique_options)) {
    unique_options <- data.table::transpose(unique_options) |>
      setnames(new = new_names)
    unique_options <- unique(unique_options)

    if (use_which) {
      unique_options <- unique_options |>
        is.na() |>
        not() |>
        apply(
          1,
          which,
          simplify = F
        )
    }

    return(unique_options)
  }
}

which_range <- function(non_list_reason_vec) {
  # helps connect selling point to first preceding update
  # update chain upto and including selling point are connected into one listing

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
check_nonsensical_listings <- function(data_connected, data_name) {
  # calc ayear/eyear from amonths/emonths
  data_connected <- copy(data_connected)[, ":="(ayear = amonths %/% 12, eyear = emonths %/% 12)]

  # table ayear vs eyear and remove name column
  ayear_eyear_table <- data_connected |>
    tabyl(ayear, eyear) |>
    select(-any_of(c("ayear", "eyear"))) |>
    as.matrix()

  # save non-modified table for saving, next step modifies in place
  table_to_save <- ayear_eyear_table

  # set upper triangle of matrix including diag as NA (these are okay to be >0)
  ayear_eyear_table[upper.tri(ayear_eyear_table, diag = T)] <- NA

  # calc sum of colsums to check that entire lower triangle of matrix is contains only 0
  if (!sum(colSums(ayear_eyear_table, na.rm = T)) == 0) {
    # save to file
    table_to_save |>
      htmlTable(rnames = F) |>
      kableExtra::save_kable(paste0(output_path, "/", data_name, "_", "ayear", "_", "eyear", ".png"))

    # there should be a better way to stop the pipeline with msg but i wasnt able to find one yet
    tar_assert_true(
      FALSE,
      msg = glue::glue("nonsensical ayear/eyear combinations found at: {unique(data_connected$latlon_utm)}")
    )

    return(NULL)
  }
}


between_helper <- function(i_value, j_value, offset) {
  abs_diff = abs(i_value - j_value)
  out <- (abs_diff < offset) | all.equal(abs_diff, offset)
  return(out)
}
