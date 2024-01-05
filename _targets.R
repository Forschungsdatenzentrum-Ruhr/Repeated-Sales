# General TODO: -----------------------------------------------------------

# add packagename before functions
# figure out what to with same_time_listings
# have pipeline run seperately for each of WK, HK, WM (make this a setting in setup)
# or have it run all of them sequentially? might be kinda hard to implement


# the classification steps appear to be a mix of dbscan and k-neigherst neighoor?
# swap out kaufpreis/kaltmiete mit preis_var to make indices functions universal

#options(error = traceback)
# Packages-Setup: ----------------------------------------------

# used during setup of pipeline
req_library <- c(
  "targets",
  "tarchetypes",
  "future",
  "future.callr",
  "fst",
  "renv",
  "rlang",
  "styler",
  "docstring"
)

# used during execution of pipeline
pipeline_library <- c(
  "here",
  "stringr",
  "dplyr",
  "tidyr",
  "data.table",
  "cli",
  "glue",
  "ggplot2",
  "haven", # reading/writing of dta files
  "tidyverse", # data manipulation/wrangeling
  "magrittr", # two sided pipe
  "fst", #
  "modelsummary",
  "janitor",
  "htmlTable"
)

suppressPackageStartupMessages({
  # used during setup of pipeline
  library(targets)
  library(tarchetypes)
  library(future)
  library(future.callr)
  library(fst)
  library(renv)
  library(rlang)
  library(styler)
  library(docstring)
  library(jsonlite)

  # used during execution of pipeline
  library(MetBrewer)
  library(here)
  library(stringr)
  library(dplyr)
  library(tidyr)
  library(data.table)
  library(cli)
  library(glue)
  library(ggplot2)
  library(haven)
  library(modelsummary)
  library(janitor)
  library(kableExtra)
  library(htmlTable)
  library(fixest)
  library(magrittr)
  library(rsmatrix)
  library(ggplot2)
  library(qs)
  library(scatterplot3d)
})


# Pipeline Settings ----------------------------------------------------------

# target options
tar_option_set(
  resources = tar_resources(
    fst = tar_resources_fst(compress = 50)
  ),
  packages = pipeline_library,
  garbage_collection = TRUE,
  storage = "worker",
  retrieval = "worker"
)

# tar_make_future() configuration:
plan(callr)

###########################################################################
# Paths and Globals -----------------------------------------------------------
###########################################################################

# Globals -------------------------------------------------------------------

# for now only can run one type at once
RED_version <- "v9"
# one of WM / WK / HK
RED_type <- "WK"


## similarity settings
categories <- c("wohnflaeche", "etage", "zimmeranzahl")


# maybe set zimmeranzahl to 1?
# resembling_offset
wohnflaeche_r_o <- 0.1
etage_r_o <- 1
zimmeranzahl_r_o <- 0.5

# exact_offset
wohnflaeche_e_o <- 0.05
etage_e_o <- 0
zimmeranzahl_e_o <- 0.5


# plot_offset
low_cutoff <- 0.1
mid_cutoff <- 1
high_cutoff <- 20
final_cutoff <- 100

wohnflaeche_ro_range <- c(
  seq(from = 0, to = low_cutoff, by = 0.02),
  seq(from = low_cutoff, to = mid_cutoff, by = 0.1),
  seq(from = mid_cutoff - 1, to = high_cutoff, by = 5),
  seq(from = high_cutoff, to = final_cutoff, by = 20)
) |> unique()

wohnflaeche_eo_range <- shift(wohnflaeche_ro_range)

wohnflaeche_eo_range <- wohnflaeche_eo_range[-1]
wohnflaeche_ro_range <- wohnflaeche_ro_range[-1]

sensitivity_suffix <- wohnflaeche_eo_range |> str_replace_all("\\.", "_")

# time offset for readability
time_offset <- fcase(
  RED_type == "WM", 3,
  RED_type %in% c("WK", "HK"), 6
)

# settings export setup
exportJSON <- data.table(
  "RED_type" = RED_type,
  "RED_version" = RED_version,
  "categories" = categories,
  "wohnflaeche_r_o" = wohnflaeche_r_o,
  "etage_r_o" = etage_r_o,
  "zimmeranzahl_r_o" = zimmeranzahl_r_o,
  "wohnflaeche_e_o" = wohnflaeche_e_o,
  "zimmeranzahl_e_o" = zimmeranzahl_e_o,
  "time_offset" = time_offset
)

# Setup Misc --------------------------------------------------------------

# logging setup
logger::log_appender(
  logger::appender_file(
    paste0(here::here("log"), "/", Sys.Date(), "_log.log")
  )
)

# tar_eval variables
federal_state_ids <- c(5)
# federal_state_ids <- 1:16
# federal_state_ids <- c(1:10,12:16)
classification_ids <- glue::glue("classification_blid_{federal_state_ids}")

curr_date <- Sys.Date() |> str_replace_all("-", "_")

# Paths -------------------------------------------------------------------

# main path, path where this file is located
main_path <- here::here()

# code-path
code_path <- here::here("R")

# data-path
data_path <- here::here("data")

markdown_path <- here::here("documentation", "markdown_")

# output path
output_path <- here::here("output", RED_type, RED_version, curr_date)

# Sourcing ----------------------------------------------------------------

# Read main files
lapply(
  list.files(
    file.path(code_path),
    pattern = "\\.R$",
    full.names = TRUE,
    all.files = FALSE
  ),
  source
)

# Read code files
for (sub_dir in c("read_", "make_", "summary_", "similarity_", "misc")) {
  lapply(
    list.files(
      file.path(code_path, glue::glue("{sub_dir}")),
      pattern = "\\.R$",
      full.names = TRUE,
      all.files = FALSE
    ),
    source
  )
}


###########################################################################
# FILE_TARGETS -----------------------------------------------------------
###########################################################################

file_targets <- rlang::list2(

  # dump settings as json file to make results reproducible
  tar_target(
    settings_used,
    output_path_json(
      output_path
    ),
    deployment = "main"
  ),
  # implement same split via tar_eval as in master
  tar_target(
    file_name,
    # generates file_name used based on version and type
    make_RED_file_name(
      data_version = RED_version,
      data_type = RED_type
    )
  ),
  ## RED data
  tar_file_read(
    RED_all_columns,
    file_name,
    # read stata file, removes labels and mutate some variables
    read_RED(!!.x),
    deployment = "main"
  ),
  # cut down RED data to only columns required for classification
  tar_fst_dt(
    RED_req_columns,
    prepare_RED(
      RED_all_columns,
      var_of_interest = c(
        ## general info
        "blid",

        ## object info
        "wohnflaeche",
        "zimmeranzahl",
        "etage",
        "balkon",

        ## mutated info
        "counting_id",
        "latlon_utm",
        "amonths",
        "emonths",
        "price_var"
      )
    ),
    deployment = "main"
  )
)


# ###########################################################################
# # FEDERALSTATE_TARGETS -------------------------------------------------------------
# ###########################################################################
# create targets for each federal states
federal_state_targets <- rlang::list2(

  # this seems slightly slower than prior usage of tar_group_by + pattern(map)
  # usage of pattern causes hash names however which makes loading difficult
  # classify data
  tar_eval(
    tar_fst_dt(
      classification_ids,
      make_classification(
        # this way of grouping causes targets problems when tracking changes
        # doesnt recognize changes to RED_req_columns and therefore doesnt rerun classification
        geo_grouped_data = RED_req_columns[.(federal_state_ids), on = "blid"]
      )
    ),
    values = rlang::list2(
      federal_state_ids = federal_state_ids,
      classification_ids = classification_ids
    )
  )
)

###########################################################################
# Markdown --------------------------------------------------------------
###########################################################################
markdown_targets <- rlang::list2(
  tar_fst_dt(
    example_markdown_data,
    make_example_markdown_data(
      geo_grouped_data = RED_req_columns[.(4), on = "blid"]
    ),
    deployment = "main"
  ),
  tar_render(
    example_markdown,
    paste0(markdown_path, "/example_markdown.Rmd")
  ),
  deployment = "main"
)

###########################################################################
# Summary --------------------------------------------------------------
###########################################################################
# arguments to create summary_tables from
# first of arg1 vector correspondences to first argument of arg2 vector
cross_tabyl_arguments <- data.table(
  arg1 = c(
    "blid",
    "sim_index",
    "blid",
    "same_time_listing"
  ),
  arg2 = c(
    "sim_index",
    "non_list_reason",
    "non_list_reason",
    "non_list_reason"
  )
)[
  ,
  target_name := paste0("summary_table", "_", arg1, "_", arg2)
]

# Tables ------------------------------------------------------------------
table_targets <- rlang::list2(

  # classification
  tar_target(
    summary_skim_numeric,
    datasummary_skim_numerical(
      classification
    )
  ),
  tar_target(
    summary_skim_cat,
    datasummary_skim_categorical(
      classification
    )
  ),
  tar_eval(
    tar_target(
      target_name,
      custom_cross_tabyl(
        classification,
        arg1 = arg1,
        arg2 = arg2
      )
    ),
    values = cross_tabyl_arguments
  ),
)
# Figures -----------------------------------------------------------------
figure_targets <- rlang::list2()


###########################################################################
# EXPORT-TARGETS -----------------------------------------------------------
###########################################################################
export_targets <- rlang::list2(
  tar_target(
    export_classification,
    export_data(
      RED_classified,
      data_version = RED_version,
      data_type = RED_type
    )
  ),
)

###########################################################################
# Price Indices -----------------------------------------------------------
###########################################################################
indices_targets <- rlang::list2(
  tar_fst_dt(
    RED_classified,
    # this needs the initial version of red with all columns since some are
    # used during regression but not during classification
    remerge_RED(
      classification,
      RED_all_columns
    )
  ),
  tar_target(
    prepared_hedonic,
    prepare_hedonic(
      RED_classified,
      data_type = RED_type
    )
  ),
  tar_target(
    hedonic_index,
    make_hedonic(
      prepared_hedonic,
      data_type = RED_type
    ),
    format = "rds"
  ),
  tar_fst_dt(
    self_merged_rs_pairs,
    prepare_repeated(
      RED_classified,
      grouping_var = "gid2019"
    )
  ),
  # use remerged RED for now, since i need some variables not in classification
  tar_target(
    repeated_index,
    make_repeated(
      self_merged_rs_pairs,
      grouping_var = "gid2019"
    ),
    format = "rds"
  ),
  # tar_target(
  #   hybrid_index,
  #   make_hybrid(
  #     RED_classified,
  #     data_type = RED_type
  #   ),
  #   format = "rds"
  # )
)


###########################################################################
# FINAL_TARGETS -----------------------------------------------------------
###########################################################################
## combine to main pipeline
rlang::list2(
  file_targets,

  # federal state targets
  federal_state_targets,

  # # combine last step of federal state targets together into single output
  tar_combine(
    classification,
    federal_state_targets[[1]],
    command = bind_rows(!!!.x),
    format = "fst_dt",
    cue = tar_cue(mode = "always")
  ),
  markdown_targets,
  table_targets,
  figure_targets,
  # export_targets,
  indices_targets
)
