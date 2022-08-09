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

#rerun this if packages are changed/missing
# full_library = c(req_library,pipeline_library)
# 
# for(package in 1:length(full_library)){
#   install.packages(full_library[package])
# 
# }

suppressPackageStartupMessages({
  #used during execution of pipeline
  library(tidyverse)
  library(here)
  library(haven)
  library(magrittr)
  
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

######################
#Sourcing
######################
# Load the R scripts with your custom functions:
lapply(list.files("R", full.names = TRUE, recursive = TRUE), source)
######################
#Pipeline
######################
#break this up less to reduce overhead
# split by plz? maybe grid ? 5x5? 1x1?
blid_targets = tar_map(
  values = list(.bl_id = 1:1),
  tar_fst_tbl(bl, load_data(filename, bl_id = .bl_id )),
  tar_group_by(plz_group, bl, plz),
  tar_group_by(coord_group, plz_group, latlon_utm, balkon),
  tar_fst_tbl(classification, classify_data(coord_group), pattern = map(coord_group))
)

list(
  tar_target(filename, construct_file_name("v6","Wk"),format = "file"),
  blid_targets,
  tar_combine(repeated,blid_targets[[4]], command = bind_rows(!!!.x), format = "fst_dt")
)






