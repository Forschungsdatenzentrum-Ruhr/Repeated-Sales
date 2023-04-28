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
# Repeated Offerings
######################
# data_type_loop ----------------------------------------------------------
for(d in 1:length(data_types)){
  
  ##load initial data
  type = data_types[d]
  log_info(paste0("Start type: ",type))
  
  

# federal_state_loop ------------------------------------------------------
  for(bl_id in 11:11){

    #log
    log_info(paste0("Start bl_id: ",bl_id))
    start_time = Sys.time()
    
    ## Var Preperation
    # declare here so they reset each iteration
    final_outer = c()
    final_list = c()
    

    
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
    
    
    
    
    
    
    
    
    