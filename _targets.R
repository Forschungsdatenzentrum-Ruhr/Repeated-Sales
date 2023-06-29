# General TODO: -----------------------------------------------------------

# use formatting of pop-forecast
# swap away from %>% ?
# add packagename before functions
# use data.table
# cluster around each baseline?


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
  "janitor"
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
})


# Pipeline Settings ----------------------------------------------------------

# target options
tar_option_set(
  resources = tar_resources(
    fst = tar_resources_fst(compress = 50)
  ),
  packages = pipeline_library,
  seed = 1,
  trust_object_timestamps = TRUE,
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
RED_version <- "v8"
RED_type <- "WM"


## similarity settings
categories = c("wohnflaeche", "etage", "zimmeranzahl")


# maybe set zimmeranzahl to 1?
#resembling_offset
wohnflaeche_r_o =  0.1
etage_r_o = 99
zimmeranzahl_r_o =  0.5

#exact_offset
wohnflaeche_e_o =  0.05
etage_e_o = 0
zimmeranzahl_e_o =  0


# time offset for readability
time_offset <- 6

# settings export setup
exportJSON = data.table(
  "RED_type" = RED_type,
  "RED_version" = RED_version,
  "categories" = categories,
  "wohnflaeche_r_o" = wohnflaeche_r_o,
  "etage_r_o" = etage_r_o,
  "zimmeranzahl_r_o" = zimmeranzahl_r_o,
  "wohnflaeche_e_o" = wohnflaeche_e_o,
  "zimmeranzahl_e_o" =  zimmeranzahl_e_o,
  "time_offset" = time_offset
)


# Paths -------------------------------------------------------------------

# data-path
data_path <- here::here("data")

# output path
output_path = here::here("output",RED_type,RED_version)


# logging setup
logger::log_appender(
  logger::appender_file(
    paste0(here::here("log"), "/", Sys.Date(), "_log.log")
  )
)


# tar_eval variablse ------------------------------------------------------
federal_state_ids = 1:16
classification_ids = glue::glue("classification_blid_{federal_state_ids}")


# Sourcing: ---------------------------------------------------------------

# Load the R scripts with your custom functions:
lapply(list.files("R", pattern = "\\.R$", full.names = TRUE), source)

###########################################################################
# FILE_TARGETS -----------------------------------------------------------
###########################################################################

file_targets <- rlang::list2(

  tar_target(
    settings_used,
    output_path_json(
      output_path
    ),
    deployment = "main"
  ),
  
  # read RED data and
  tar_file_read(
    RED,
    # generates file_name used, version and type can be specified
    make_RED_file_name(
      data_version = RED_version,
      data_type = RED_type
    ),
    # read stata file, removes labels and create individual data for fs
    read_RED(!!.x,
      var_of_interest = c(
        ## general info
        "blid",
        "ajahr",
        "ejahr",
        "amonat",
        "emonat",

        ## object info
        "wohnflaeche",
        "zimmeranzahl",
        "etage",
        "mietekalt",
        "kaufpreis",
        "balkon",
        "lat_utm",
        "lon_utm"
      )
    ),
    deployment = "main"
  )
)


# ###########################################################################
# # FEDERALSTATE_TARGETS -------------------------------------------------------------
# ###########################################################################
#
## create targets for each federal states
federal_state_targets <- rlang::list2(
  # group data by federal state
  # this could be any group larger than lat+lon
  # smaller granularity leads to more intermediary files and therefore scaling issues
  
  
  #this seems slightly slower than prior usage of tar_group_by + pattern(map)
  # usage of pattern causes hash names however which makes loading difficult
  # classify data
  tar_eval(
    tar_fst_dt(
      classification_ids,
      make_classification(
        geo_grouped_data = RED[.(federal_state_ids), on = "blid"]
      )
    ),
    values = rlang::list2(
      federal_state_ids = federal_state_ids,
      classification_ids = classification_ids
    )
   )

)


###########################################################################
# Summary --------------------------------------------------------------
###########################################################################

cross_tabyl_arguments = data.table(
    arg1 = c("blid","sim_index","blid"), 
    arg2 = c("sim_index","non_list_reason","non_list_reason")
)[,
  target_name := paste0("summary_table","_",arg1,"_",arg2)
]

summary_targets = rlang::list2(
  
  tar_target(
    summary_skim_numeric,
    datasummary_skim_numerical(
      combined_federal_states
    )
  ),
  tar_target(
    summary_skim_cat,
    datasummary_skim_categorical(
      combined_federal_states
    )
  ),
  tar_eval(
    tar_target(
      target_name,
      custom_cross_tabyl(
        combined_federal_states,
        arg1 = arg1,
        arg2 = arg2
      )
    ),
    values = cross_tabyl_arguments
  )
  
  
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
    combined_federal_states,
    federal_state_targets[[length(federal_state_targets)]],
    command = bind_rows(!!!.x),
    format = "fst_dt"
  ),
  summary_targets
)


###########################################################################
# PLOTTING ----------------------------------------------------------------
###########################################################################
