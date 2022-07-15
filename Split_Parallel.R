######################
#ToDo
######################
## code
# change dta input to fst
# expand comments and explain sources + functions
# wrap loop into catch for logging
# find a way to log progress; should help debug
## content
# similarity comparison during classification
# strict non duplicates during preselection
# kernels ? 

######################
#Notation
######################
# parent: initial (successful) offering of object i
# child: repeated (successful) offering of i
# update: unsuccessful re listing of offering

######################
#Package Install and Load
######################
packages_install = function(library_string){
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(library_string, character.only = TRUE)    
  print("Packages fully installed and loaded")
}
######################
#Packages
######################
library_string = c("here", # creating dynamic paths based on script location
                   "haven", # reading/writing of dta files
                   "tidyverse", # data manipulation/wrangeling
                   "doParallel", #parallel processing
                   "foreach", #parallel looping
                   "magrittr", #two sided pipe
                   "logger"
)

# load packages
packages_install(library_string)
######################
#Paths
######################
path = paste0(here(),"/")
readpath = "M:/_FDZ/RWI-GEO/RWI-GEO-RED/daten/On-site/v6/"
writepath = paste0(path,"Output/")

######################
#Functions
######################
# contains classification steps
## update removal:
# loop over unique living space to id potential parent/child structure
# within structure drop all children shorter than (currently) t months removed from parent
## repeated id:
# using first recorded non-update as a baseline
# check if subsequent offerings at coordinate are similar in room number and floor
# depending on similarity assign repeated id: true, half or resembling (1,1,3)
source(paste0(path,"Classification.R"))

# contains helperfunctions:
# i) rangeChecker
# ii) printer (unused)
source(paste0(path,"HelperFunctions.R"))

######################
#Logging
######################
# create log file with current date
log_name = paste0("log_",Sys.Date(),".txt")
log_file = paste0(writepath,log_name)

# write logs to both console and file
log_appender(appender_tee(log_file))

######################
#Declarations
######################
## parallel computing
# detect number of cores present and assign as workers
numCores = detectCores()
registerDoParallel(cores = numCores)

## variables
# declare necessary non-missing entries in the data
var_of_interest = c("wohnflaeche","amonths","emonths","zimmeranzahl","etage","price_var")
# declare data types out of: wk, wm, hk
data_types = c("Wk","Wm")

######################
# Repeated Offerings
######################
# data_type_loop ----------------------------------------------------------
for(d in 1:length(data_types)){
  
  ##load initial data
  type = data_types[d]
  log_info(paste0("Start type: ",type))
  t_data = read_dta(paste0(readpath,type,"_allVersionsLabels.dta"))
  

# federal_state_loop ------------------------------------------------------
  for(bl_id in 11:11){
    
    #log
    log_info(paste0("Start bl_id: ",bl_id))
    start_time = Sys.time()
    
    ## Var Preperation
    # declare here so they reset each iteration
    final_outer = c()
    final_list = c()
    
    ##Data Preperation
    #shrink data to necessary
    bl = t_data %>%
      filter(
        # subset to federal state
        blid == bl_id,
        # drop missing coordinates
        !lat_utm < 0 | !lon_utm < 0
      ) %>%
      select(
        # drop text variables
        -freiab,
        -mietekaution,
        -courtage
      ) %>%
      mutate(
        ## gen variables
        # coordinate combination
        latlon_utm = paste0(lat_utm,lon_utm),
        #transform years into months and add running years months
        amonths = ajahr * 12 + amonat,
        emonths = ejahr * 12 + emonat,
        # merge price and rent into one variable
        price_var = pmax(mietekalt,kaufpreis),
        # create unique id for entire data
        counting_id = 1:n(),
        # get enddate of date
        data_end_date = max(emonths)
      )
    
    # recode and drop relevant missings
    bl[bl < 0] = NA
    bl %<>% drop_na(var_of_interest)
    
    unique_latlon = unique(bl$latlon_utm)
    
# coordinate_parallel_loop ------------------------------------------------
    final_outer = foreach(latlon = unique_latlon, .combine = "rbind", .inorder = F, .packages = library_string) %dopar%{

      #subset by unique coordinate combination
      outer_dummy = filter(bl, latlon_utm == latlon)
      
      #sort by offering start
      outer_dummy = outer_dummy[order(outer_dummy$amonths),]
      
      #get unique values for living space
      unique_wohnflaeche = unique(outer_dummy$wohnflaeche)
      
      #classify coordinate
      classification(unique_wohnflaeche = unique_wohnflaeche, outer_dummy = outer_dummy)
  
    }
    
    #log
    log_info(paste0("End bl_id: ",bl_id))
    t_difference = round(difftime(Sys.time(),start_time, units = "mins"),4)
    log_info(paste0("Runtime: ",t_difference,"mins"))
    
    #count occurences of parent objects
    final_outer = distinct(final_outer)
    count_parents = final_outer %>% count(obj_parent)
    keep_parents = count_parents[1][count_parents[2] > 1]
    
    #drop if object is only its own parent
    final_list = final_outer %>% filter(obj_parent %in% keep_parents) %>% arrange(obj_parent,counting_id)
    
    #save file
    file_name = paste0(type,"_",bl_id,"_repeated_offerings_",Sys.Date(),".dta")
    write_dta(final_list,paste0(writepath,file_name))
  }
}
stopImplicitCluster()

q(save = "no")    
    
    
    
    
    
    
    
    
    