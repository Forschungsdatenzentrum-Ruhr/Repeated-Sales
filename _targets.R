# General TODO: -----------------------------------------------------------

# add packagename before functions
# figure out what to with same_time_listings
# clean up the indices

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
  "htmlTable",
  "rsmatrix",
  "kableExtra",
  "fixest",
  "qs",
  "scatterplot3d",
  "jsonlite"
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
options(warn = 1)
options(scipen=999)
options("modelsummary_format_numeric_latex" = "plain")

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

static_RED_types <- c("WK", "WM", "HK") #not tried yet
RED_version <- "v9"

## similarity settings
categories <- c("wohnflaeche", "etage", "zimmeranzahl")

# time offset for readability
time_offset <- 6

# exact_offset
wohnflaeche_e_o <- 0.05
etage_e_o <- 0
zimmeranzahl_e_o <- 0.5

# plot
base_quarter = "2010-01-01"

# settings export setup
exportJSON <- data.table(
  "RED_version" = RED_version,
  "categories" = categories,
  "etag_e_o" = etage_e_o,
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

# Paths -------------------------------------------------------------------

# main path, path where this file is located
main_path <- here::here()

# code-path
code_path <- here::here("R")

# data-path
data_path <- here::here("data")

markdown_path <- here::here("documentation", "markdown_")

# output paths
static_output_path <- here::here("output")

output_path <- here::here("output")

for (i in static_RED_types) {
  if (!dir.exists(file.path(output_path, i))) {
    dir.create(file.path(output_path, i), recursive = TRUE)
  }
}

# Sourcing ----------------------------------------------------------------

# Read code files
for (sub_dir in c("RED_", "make_", "summary_", "similarity_", "misc","plot_")) {
  lapply(
    list.files(
      file.path(code_path, glue::glue("{sub_dir}")),
      pattern = "\\.R$",
      full.names = TRUE,
      all.files = FALSE,
      recursive =TRUE
    ),
    source
  )
}
# ###########################################################################
# # tar_eval constants------------------------------------------------------
# ###########################################################################
# # tar_eval variables
federal_state_ids <- 1:16
classification_ids <- glue::glue("classification_blid_{federal_state_ids}")

# # constants used for branching in tar_eval
static_RED_file_names <- glue::glue("{static_RED_types}_file_name")
static_RED_full_data <- glue::glue("{static_RED_types}_full_data")
static_RED_req_data <- glue::glue("{static_RED_types}_req_data")
static_RED_classified <- glue::glue("{static_RED_types}_classified")
static_RED_subset_classified <- glue::glue("{static_RED_types}_subset_classified") 
static_RED_classification = glue::glue("{static_RED_types}_classification")
static_prepared_hedonic = glue::glue("{static_RED_types}_prepared_hedonic")
static_hedonic_index = glue::glue("{static_RED_types}_hedonic_index")
static_prepared_repeated = glue::glue("{static_RED_types}_prepared_repeated")
static_repeated_index = glue::glue("{static_RED_types}_repeated_index")
static_hybrid_index = glue::glue("{static_RED_types}_hybrid_index")
static_combined_index = glue::glue("{static_RED_types}_combined_index")
static_split_index = glue::glue("{static_RED_types}_split_index")
static_export = glue::glue("{static_RED_types}_export")

# plots
static_plot_combined = glue::glue("{static_RED_types}_plot_combined")
static_plot_split = glue::glue("{static_RED_types}_plot_split")

# extend some constants to match lengths needed
dynamic_federal_state_ids <- rep(federal_state_ids, length(static_RED_types))
dynamic_RED_req_data = rep(static_RED_req_data, each = length(federal_state_ids))
dynamic_RED_types <- rep(static_RED_types, each = length(federal_state_ids))
dynamic_RED_classification_ids <- glue::glue("{dynamic_RED_types}_classification_blid_{dynamic_federal_state_ids}")
 

###########################################################################
# RED -----------------------------------------------------------
###########################################################################

RED_targets <- rlang::list2(

  # dump settings as json file to make results reproducible
  tar_target(
    settings_used,
    output_path_json(
      output_path
    ),
    deployment = "main"
  ),
  tar_eval(
    list(
      # generates file_name used based on version and type
      tar_target(
        RED_file_names,
        make_RED_file_name(
          data_version = RED_version,
          data_type = RED_types
        )
      ),
      # read stata file, removes labels and mutate some variables
      tar_file_read(
        RED_full_data,
        RED_file_names,
        read_RED(!!.x),
        deployment = "worker"
      ),
      # cut down RED data to only columns required for classification
      tar_fst_dt(
        RED_req_columns,
        prepare_RED(
          RED_full_data,
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
        deployment = "worker"
      )
    ),
    values = list(
      RED_types = static_RED_types,
      RED_file_names = rlang::syms(static_RED_file_names),
      RED_full_data = rlang::syms(static_RED_full_data),
      RED_req_columns = rlang::syms(static_RED_req_data)
    )
  )
)

###########################################################################
# FEDERALSTATE -------------------------------------------------------------
###########################################################################
# create targets for each federal states
federal_state_targets <- rlang::list2(

  # this seems slightly slower than prior usage of tar_group_by + pattern(map)
  # usage of pattern causes hash names however which makes loading difficult
  # classify data
  tar_eval(
    tar_fst_dt(
      RED_classification_ids,
      make_classification(
        # this way of grouping causes targets problems when tracking changes
        # doesnt recognize changes to RED_req_columns and therefore doesnt rerun classification
        geo_grouped_data = RED_req_columns[.(RED_federal_state_ids), on = "blid"]
      ),
      deployment = "worker"
    ),
    values = rlang::list2(
      RED_federal_state_ids = dynamic_federal_state_ids,
      RED_classification_ids = rlang::syms(dynamic_RED_classification_ids),
      RED_req_columns = rlang::syms(dynamic_RED_req_data)
    )
  )
)

# ###########################################################################
# # CLASSIFICATION -------------------------------------------------------------
# ###########################################################################
## combination helpers
# effectively just split the list into halves, since types are guaranteed to be in order
RED_type_count = length(static_RED_types)
WK_indices <- seq(to = length(dynamic_RED_req_data) / RED_type_count)
WM_indices <- length(dynamic_RED_req_data) / RED_type_count + WK_indices
HK_indices = length(dynamic_RED_req_data) / RED_type_count + WM_indices

classification_targets = rlang::list2( 
   # combine last step of federal state targets together into single output
  tar_combine(
    WK_classification,
    federal_state_targets[[1]][WK_indices],
    command = bind_rows(!!!.x),
    format = "fst_dt"
  ),
  tar_combine(
    WM_classification,
    federal_state_targets[[1]][WM_indices],
    command = bind_rows(!!!.x),
    format = "fst_dt"
  ),
  tar_combine(
    HK_classification,
    federal_state_targets[[1]][HK_indices],
    command = bind_rows(!!!.x),
    format = "fst_dt"
  ),
)

###########################################################################
# PRICE INDICES -----------------------------------------------------------
###########################################################################
indices_targets <- rlang::list2(
  tar_eval(
    list(
      # further process needs the initial version of red with all columns since some are
      # used during regression but not during classification
      tar_fst_dt(
        RED_classified,
        remerge_RED(
          classification,
          RED_full_data
        )
      ),
      tar_fst_dt(
        RED_subset_classified,
        subset_RED(
          RED_classified
        )
      ),
      # do REDX-esque preperation of the hedonic data
      tar_target(
        prepared_hedonic,
        prepare_hedonic(
          RED_subset_classified,
          data_type = RED_types
        )
      ),
      # hedonic regression + index calc
      tar_target(
        hedonic_index,
        make_hedonic(
          prepared_hedonic,
          data_type = RED_types
        ),
        format = "rds"
      ),
      tar_fst_dt(
        prepared_repeated,
        prepare_repeated(
          RED_subset_classified,
          grouping_var = "gid2019"
        )
      ),
      # use remerged RED for now, since i need some variables not in classification
      tar_target(
        repeated_index,
        make_repeated(
          prepared_repeated,
          grouping_var = "gid2019"
        ),
        format = "rds"
      ),
      tar_target(
        hybrid_index,
        make_hybrid(
          RED_subset_classified,
          prepared_repeated,
          data_type = RED_types
        ),
        format = "rds"
      ),
      tar_target(
        combined_index,
        make_combined(
          repeated_index,
          hybrid_index,
          hedonic_index
        ),
        format = "rds"
      ),
      tar_target(
        split_index,
        make_split(
          repeated_index,
          hybrid_index,
          hedonic_index
        ),
        format = "rds"
      )
    ),
    values = rlang::list2(
      RED_types = static_RED_types,
      RED_subset_classified = rlang::syms(static_RED_subset_classified),
      RED_classified = rlang::syms(static_RED_classified),
      RED_full_data = rlang::syms(static_RED_full_data),
      classification = rlang::syms(static_RED_classification),
      prepared_hedonic = rlang::syms(static_prepared_hedonic),
      hedonic_index = rlang::syms(static_hedonic_index),
      prepared_repeated = rlang::syms(static_prepared_repeated),
      repeated_index = rlang::syms(static_repeated_index),
      hybrid_index = rlang::syms(static_hybrid_index),
      combined_index = rlang::syms(static_combined_index),
      split_index = rlang::syms(static_split_index)

    )
  )
)

###########################################################################
# MARKDOWN --------------------------------------------------------------
###########################################################################
markdown_targets <- rlang::list2(
  tar_fst_dt(
    example_markdown_data,
    make_example_markdown_data(
      geo_grouped_data = WK_req_data[.(4), on = "blid"]
    ),
    deployment = "main"
  ),
  tar_render(
    example_markdown,
    paste0(markdown_path, "/example_markdown.Rmd")
  ),
  deployment = "main"
)
# ###########################################################################
# # PLOT --------------------------------------------------------------
# ###########################################################################
plot_targets = rlang::list2(
  tar_eval(
    list(
      # plot combined
      tar_target(
        p_combined,
        plot_combined(
          combined_index,
          data_type = RED_types
        )
      ),
      # plot split
      tar_target(
        p_split,
        plot_split(
          split_index,
          data_type = RED_types
        )
      )
    ),
    values = rlang::list2(
      RED_types = static_RED_types,
      combined_index = rlang::syms(static_combined_index),
      split_index = rlang::syms(static_split_index),
      p_split = rlang::syms(static_plot_split),
      p_combined = rlang::syms(static_plot_combined)
    )
  )

)


# ###########################################################################
# # SUMMARY --------------------------------------------------------------
# ###########################################################################
# # arguments to create summary_tables from
# # first of arg1 vector correspondences to first argument of arg2 vector
# cross_tabyl_arguments <- data.table(
#   arg1 = c(
#     "blid",
#     "sim_index",
#     "blid",
#     "same_time_listing"
#   ),
#   arg2 = c(
#     "sim_index",
#     "non_list_reason",
#     "non_list_reason",
#     "non_list_reason"
#   )
# )[
#   ,
#   target_name := paste0("summary_table", "_", arg1, "_", arg2)
# ]

# # Tables ------------------------------------------------------------------
# table_targets <- rlang::list2(

#   # classification
#   tar_target(
#     summary_skim_numeric,
#     datasummary_skim_numerical(
#       classification
#     )
#   ),
#   tar_target(
#     summary_skim_cat,
#     datasummary_skim_categorical(
#       classification
#     )
#   ),
#   tar_eval(
#     tar_target(
#       target_name,
#       custom_cross_tabyl(
#         classification,
#         arg1 = arg1,
#         arg2 = arg2
#       )
#     ),
#     values = cross_tabyl_arguments
#   ),
# )
# # Figures -----------------------------------------------------------------
# figure_targets <- rlang::list2()


###########################################################################
# EXPORT-TARGETS -----------------------------------------------------------
###########################################################################
export_targets <- rlang::list2(
  tar_eval(
    tar_target(
      export_classification,
      export_data(
        RED_classified,
        data_type = RED_types
      )
    ),
    values = rlang::list2(
      RED_types = static_RED_types,
      RED_classified = rlang::syms(static_RED_classified),
      export_classification = rlang::syms(static_export)
    )
  )
)

###########################################################################
# FINAL_TARGETS -----------------------------------------------------------
###########################################################################

## combine to main pipeline
rlang::list2(
  RED_targets,
  federal_state_targets,
  markdown_targets,
  classification_targets,
  plot_targets,
  export_targets,
  indices_targets
)
