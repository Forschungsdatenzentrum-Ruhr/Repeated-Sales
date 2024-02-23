#' Helper and Debug Functions, WIP-Section
#'
#' @description
#' Universal functions used during different steps of pipeline aswell as WIP-
#' Section and debugging
#'

output_path_json <- function(output_path = NA) {
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

which_range <- function(non_list_reason_vec = NA) {
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

custom_progress_bar <- function(classification_type = NA, .GRP = NA, .GRPN = NA, mod = 100) {
  # the .envir argument causes the progress_bar used to be the global one
  if (.GRP == 1) {
    start_time <<- Sys.time()
    # Initialization of progress bar and some stats
    cli::cli_alert_info(glue::glue("Starting {classification_type} at {start_time}"))
    cli::cli_alert_info(glue::glue("Total groups: {.GRPN}"))
    cli::cli_progress_bar("  Classifying ...", total = (.GRPN / mod), .envir = parent.frame(n = sys.nframe()))
  } else if (.GRP == .GRPN) {
    # Finish and cleanup
    cli::cli_alert_success(glue::glue("Finished {classification_type} after {format(Sys.time() - start_time, format = '%H:%M:%S')}"))
    cli::cli_progress_done(.envir = parent.frame(n = sys.nframe()))
  } else if (!(.GRP %% mod)) {
    # Update everytime mod times x is hit
    cli::cli_progress_update(.envir = parent.frame(n = sys.nframe()))
  }

  return(NULL)
}
check_nonsensical_listings <- function(data_connected = NA, data_name = NA) {
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

make_var = function(data_type){

  if (data_type == "WK") {
    depVar <- "ln_flatprice_sqm"
    indepVar <- c(

      # raw
      "balkon",
      "garten",
      "einbaukueche",
      "gaestewc",
      "aufzug",
      "keller",
      "betreut",
      "ausstattung",
      "zimmeranzahl",

      # mutated
      "declared_wohngeld",
      "baujahr_cat",
      "first_occupancy",
      "num_floors",
      "floors_cat"
    )
    fixed_effects <- c("gid2019", "ejahr")
      
    } else if (data_type == "WM") {
      depVar <- "ln_rent_sqm"
      indepVar <- c(

      # raw
      "balkon",
      "garten",
      "einbaukueche",
      "gaestewc",
      "aufzug", # this isnt in REDX; why?
      "keller",
      "ausstattung",
      "zimmeranzahl",

      # mutated
      "baujahr_cat",
      "first_occupancy"
    )
    } else if (data_type == "HK") {
    depVar <- "ln_houseprice_sqm"

    indepVar <- c(

      # raw
      "gaestewc",
      "einliegerwohnung",
      "ausstattung",
      "zimmeranzahl",

      # mutated
      "baujahr_cat",
      "first_occupancy",
      "plotarea_cat",
      "type_cat"
    )
    fixed_effects <- c("gid2019", "ejahr")

  }
  list_var = list(depVar = depVar, indepVar = indepVar, fixed_effects = fixed_effects)
  return(list_var)
}

make_date_quarter = function(input_data){
  # reverse year to month conversion done during initial reading since subsequent functions require dates
  out <- copy(input_data)[,
    ":="(
      year = emonths %/% 12,
      month = emonths - ((emonths %/% 12) * 12)
    )
  ][
    # december is converted to an additional year, is this already a problem before this?
    # maybe use yearmon from zoo instead, shouldnt be a big change
    month == 0,
    ":="(
      month = 12,
      year = year - 1
    )
  ][, ":="(
    # sprintf is used to pad leading zeros for months while pasteing at the same time
    # %d means digits
    # %02d means digit with leading zeros until length 2
    date_month = sprintf(
      "%d-%02d-01",
      year,
      month
    ) |> as.Date(format = "%Y-%m-%d"))][,
    ":="(
    date_quarter =  sprintf(
      "%d-%02d-01",
      year,
      quarter(date_month)
    ) |> as.Date(format = "%Y-%m-%d"),
    year = NULL,
    month = NULL,
    emonths = NULL
  )]#
  return(out)

}