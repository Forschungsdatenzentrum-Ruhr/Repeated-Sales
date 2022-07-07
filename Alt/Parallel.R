######################
#Package Install Function
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
                   "magrittr" #two sided pipe
)
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
rangeChecker = function(test_value,baseline_value,offset_value,offset_type="multi"){
  
  test_value = as.numeric(test_value)
  baseline_value = as.numeric(baseline_value)
  offset_value = as.numeric(offset_value)
  
  range = case_when(
    offset_type == "add" ~ c(baseline_value - offset_value, baseline_value + offset_value),
    offset_type == "multi" ~ c(baseline_value*(1 - offset_value), baseline_value*(1 + offset_value)),
    TRUE ~ c(0,0)
  )
  check = between(test_value,range[1],range[2])
  return(check)
}

printer = function(value,by=50){
  if(value %% by == 0){
    print(value)
  }
}

######################
#Declarations
######################
#vars are allowed to deviate up to this factor in both directions
wohnflaeche_offset_factor = 0.1
etage_offset_factor = 99
zimmeranzahl_offset_factor = 0.5
time_offset_factor = 6

var_of_interest = c("wohnflaeche","amonths","emonths","zimmeranzahl","etage","price_var")
data_types = c("Wk","Wm")
numCores = 20
registerDoParallel(numCores)
######################
#Prep
######################
for(d in 1:length(data_types)){
  
  #load inital data
  type = data_types[d]
  data = read_dta(paste0(readpath,type,"_allVersionsLabels.dta"))
  
  for(bl_id in 11:11){
    
    #Var Preperation
    final_outer = c()
    final_inner = c()
    final_list = c()
    
    #Counter Preperation
    states =  c("Initial with complete coordinates","Post missing drop","Post update drop","Post classification drop","Post duplicate drop")
    obs_counter = data.frame(state = states, count = rep(0,length(states)))
    
    #Data Preperation
    #shrink data to necessary
    bl = data %>%
      filter(
        # subset to federal state
        blid == bl_id,
        # missings coordinates
        !lat_utm < 0 | !lon_utm < 0
      ) %>%
      select(
        -freiab,
        -mietekaution,
        -courtage
      ) %>%
      #new var
      mutate( 
        #coordinate combination
        latlon_utm = paste0(lat_utm,lon_utm),
        #transform years into months and add running years months
        amonths = ajahr * 12 + amonat,
        emonths = ejahr * 12 + emonat,
        price_var = pmax(mietekalt,kaufpreis),
        counting_id = 1:n(),
        data_end_date = max(emonths)
      ) 
    bl[bl < 0] = NA
    
    #drop missings
    bl %<>% drop_na(var_of_interest)
    
    #Loop Preperation
    unique_latlon = unique(bl$latlon_utm)
    data_end_date = max(bl$emonths)
    
    ######################
    #Classifcations
    ######################
    for(i in 1:length(unique_latlon)){
      
      printer(i,by = 10000)
      #subset by unique coordinate combination
      outer_dummy = filter(bl, latlon_utm == unique_latlon[i])
      
      #sort by offering start
      outer_dummy = outer_dummy[order(outer_dummy$amonths),]
    
      #catch coordinates only incomplete key variables
      if(nrow(outer_dummy) <= 1){next}
      
      #get unique values for living space and drop missings
      unique_wohnflaeche = unique(zap_labels(outer_dummy$wohnflaeche))
      
      #observation counter. this includes NAs and coordinates with less than 2 observations
      obs_counter$count[obs_counter$state == "Post missing drop"] = obs_counter$count[obs_counter$state == "Post missing drop"] + nrow(outer_dummy)
      
      #for(j in 1:length(unique_wohnflaeche)){
      x = foreach(wohn_j = unique_wohnflaeche, .combine = "rbind", .inorder = F, .packages = library_string) %dopar%{

        ##subset by range match in wohnflaeche
        inner_dummy = filter(outer_dummy, rangeChecker(wohnflaeche, wohn_j, wohnflaeche_offset_factor, "multi"))
      
        #cutoff all preceding offerings
        #these cannot be repeated offerings since they were offered before candidate
        inner_dummy = inner_dummy[match(wohn_j,inner_dummy$wohnflaeche):length(inner_dummy$wohnflaeche),]
        
        #Updates
        ######################   
        ##gen time differences
        inner_dummy %<>% mutate(
          
          #to last date in data
          td_to_end = (as.numeric(data_end_date) - as.numeric(inner_dummy$amonths)),
          #of leading offering
          td_of_lead = (lead(as.numeric(inner_dummy$amonths)) - as.numeric(inner_dummy$emonths))
        )
      
        #replace last td_of_lead with td_to_end
        inner_dummy$td_of_lead[is.na(inner_dummy$td_of_lead)] = inner_dummy$td_to_end[is.na(inner_dummy$td_of_lead)]
        
        #drops updates
        inner_dummy %<>% filter(
          .$td_of_lead >= as.numeric((time_offset_factor))
          
          ##intermediary cleanup    
        ) %>% select(
          #drop unused columns
          -td_to_end
        )
      
        #observation counter
        #obs_counter$count[obs_counter$state == "Post update drop"] = obs_counter$count[obs_counter$state == "Post update drop"] + nrow(inner_dummy)
        
        #Repeated
        ###################### 
        #extract inital offering
        baseline = inner_dummy[1,]
        
        ##gen similarity dummys
        inner_dummy %<>% mutate(
          
          #rooms
          zimmeranzahl_similar = rangeChecker(inner_dummy$zimmeranzahl, baseline$zimmeranzahl, zimmeranzahl_offset_factor, "add"),
          #floors
          etage_similar = rangeChecker(inner_dummy$etage, baseline$etage, etage_offset_factor, "add")
          
        ) %>% mutate(
          ##gen repeated indicator
          repeated_id = case_when(
            
            #is same offering
            .$counting_id == baseline$counting_id ~ "0",
            
            #both match; true repeated offering
            .$etage == baseline$etage & .$zimmeranzahl == baseline$zimmeranzahl ~ "1",
            
            #floor matches, rooms similar; half-true repeated offering
            #change to 1 later
            .$etage == baseline$etage & .$zimmeranzahl_similar ~ "1",
            
            #both are similar, resembling repeated offering
            .$etage_similar & .$zimmeranzahl_similar ~ "3",
            
            #no matches
            TRUE ~ "4"
          ),
          ##gen proposed object parent
          #shouldnt happen??
          obj_parent = ifelse(repeated_id != 4,baseline$counting_id,NA),
          
          ##gen price difference to parent
          #this needs to be variable depending on object type
          pd_to_parent = price_var - baseline$price_var,
          
          ##gen time difference to parent
          td_start_to_parent_end =  (as.numeric(amonths) - as.numeric(baseline$emonths)),
          td_end_to_parent_end =  (as.numeric(emonths) - as.numeric(baseline$emonths))
        )
      
        ##final cleanup        
        inner_dummy %>% 
          
          #drop offerings without parent
          filter(!is.na(obj_parent)) %>%
          
          #changed var
          mutate(
            changed_ausstattung = case_when(as.numeric(ausstattung) - lag(as.numeric(ausstattung)) > 0 ~ "1", TRUE ~ "0"),
            changed_objektzustand = case_when(as.numeric(objektzustand) - lag(as.numeric(objektzustand)) > 0 ~ "1", TRUE ~ "0"),
            changed_heizungsart = case_when(as.numeric(heizungsart) - lag(as.numeric(heizungsart)) > 0 ~ "1", TRUE ~ "0"),
            changed_einbaukueche = case_when(as.numeric(einbaukueche) - lag(as.numeric(einbaukueche)) > 0 ~ "1", TRUE ~ "0"),
            changed_garten = case_when(as.numeric(garten) - lag(as.numeric(garten)) > 0 ~ "1", TRUE ~ "0"),
            changed_keller = case_when(as.numeric(keller) - lag(as.numeric(keller)) > 0 ~ "1", TRUE ~ "0")
          )
      }
        #append to master and remove wohnflaeche subset array
        #if(nrow(final_inner) > 1){
        #  final_outer = rbind(final_outer,final_inner)
        #}
        #observation counter
        #obs_counter$count[obs_counter$state == "Post classification drop"] = obs_counter$count[obs_counter$state == "Post classification drop"] + nrow(final_inner)

      }
      #remove unique coordination combination subset array
      rm(outer_dummy)
  }
    #count occurences of parent objects
    final_outer = distinct(final_outer)
    count_parents = final_outer %>% count(obj_parent)
    keep_parents = count_parents[1][count_parents[2] > 1]
    #drop if object is only its own parent
    final_list = final_outer %>% filter(obj_parent %in% keep_parents) %>% arrange(obj_parent,counting_id)
    
    obs_counter$count[obs_counter$state =="Post duplicate drop"] = nrow(final_list)
    
    file_name = paste0(type,"_",bl_id,"_repeated_offerings_",Sys.Date(),".dta")
    write_dta(final_list,paste0(writepath,file_name))
    write.csv2(obs_counter,paste0(writepath,type,"_",bl_id,"_obs_counter.csv"))
  }
}    

######################
#Cleanup 
######################
#rm(list = setdiff(setdiff(ls(),c("readpath","writepath","path")), lsf.str()))




# #graveyard
# a = bl_repeated$wohnflaeche
# b = filter(bl_repeated, latlon_utm =="5818930.46282602793279.062734661")
# 
# exactChecker = function(value,test_value){
#   sum(value == test_value) >= 2
# }
# vec_wohnflaeche = b$wohnflaeche
# rm_updates = function(vec_wohnflaeche){
#   uq = unique(vec_wohnflaeche)
#   match_type = ifelse(sapply(uq, exactChecker, test_value = vec_wohnflaeche),"exact","range")
#   out = merge(vec_wohnflaeche,uq)
#   return(out)
# }
# 
# rm_updates(b$wohnflaeche)

