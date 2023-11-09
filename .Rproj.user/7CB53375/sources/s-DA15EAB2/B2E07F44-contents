######################
#Package Install and Load
######################
#stuff needs to be hard installed for it to be available to parallel
#there has to be a better way to do this

#used during setup of pipeline
req_library = c(
 "targets",
 "tarchetypes",
 "future",
 "future.callr"
)

#used during execution of pipeline
pipeline_library = c(
  "here", # creating dynamic paths based on script location
  "haven", # reading/writing of dta files
  "tidyverse", # data manipulation/wrangeling
  "magrittr", #two sided pipe
  "fst" # logging utilites
)

suppressPackageStartupMessages({
  #used during execution of pipeline
  library(tidyverse)
  library(here)
  library(haven)
  library(magrittr)
  library(vegan)
  
  #used during setup of pipeline
  library(targets)
  library(tarchetypes)
  library(future)
  library(future.callr)
  library(fst)
  library(renv)
}
)


######################
#Options and parallel
######################
# Set target options:
tar_option_set(
  resources = tar_resources(
    fst = tar_resources_fst(compress = 100)
  ),
  packages =  pipeline_library
)

# tar_make_future() configuration:
plan(callr)

# script settings
range_offsets <<- tibble::tribble(
  ~rowname, ~resembling_offset, ~exact_offset, ~offset_type,
  "wohnflaeche", 0.1, 0.05, "multi",
  "etage", 99, 0, "add",
  "zimmeranzahl", 0.5, 0, "add",
  "time", 0, 6, NA
)
# extract time offset for readability
time_offset <- range_offsets$exact_offset[range_offsets$rowname == "time"]



######################
#Sourcing
######################
# Load the R scripts with your custom functions:
lapply(list.files("R", full.names = TRUE, recursive = TRUE), source)

######################
#Pipeline
######################

## create targets for each federal states
blid_targets = tar_map(
  # federal state static ids   
  values = list(.bl_id = 1:1),
  
  # load data, do basic cleaning
  tar_fst_tbl(bl, load_data(filename, bl_id = .bl_id )),
  
  # group data by zip code
  # this could be any group larger than lat+lon
  # smaller granularity leads to more intermediary files and therefore scaling issues
  tar_group_by(plz_group, bl, plz),
  
  # classify data
  tar_fst_tbl(classification, classify_data(plz_group), pattern = map(plz_group))
)

## combine to main pipeline
list(
  # generates filename used, version and type can be specified
  tar_target(filename, construct_file_name("v6","Wk"),format = "file"),
  
  #federal state targets
  blid_targets,
  
  # combine last step of federal state targets together into single output
  tar_combine(repeated,blid_targets[[3]], command = bind_rows(!!!.x), format = "fst_dt")
)






