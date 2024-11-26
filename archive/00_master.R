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
                   "doParallel"
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

final_outer = c()
final_inner = c()
final_list = c()

states =  c("Initial with complete coordinates","Post missing drop","Post update drop","Post classification drop","Post duplicate drop")
obs_counter = data.frame(state = states, count = rep(0,length(states)))
######################
#Prep
######################
wk_data = read_dta(paste0(readpath,"Wm_allVersionsLabels.dta"))

#shrink data to necessary
berlin_only =
  wk_data %>%
  filter(
    # berlin only
    blid == 11,
    #missings coordinates
    !lat_utm < 0 | !lon_utm < 0
  ) %>%
  #select(mietekalt,kaufpreis,wohnflaeche,etage,zimmeranzahl,ajahr,amonat,ejahr,emonat,lat_utm,lon_utm) %>%
  #new var
  mutate(
    #coordinate combination
    latlon_utm = paste0(lat_utm,lon_utm),
    #transform years into months and add running years months
    amonths = ajahr * 12 + amonat,
    emonths = ejahr * 12 + emonat,
    price_var = pmax(mietekalt,kaufpreis)
  ) %>%
  select(
    -freiab,
    -mietekaution,
    -courtage
)

rm(wk_data)

obs_counter$count[obs_counter$state == "Initial with complete coordinates"] = nrow(berlin_only)

unique_latlon = unique(berlin_only$latlon_utm)
berlin_only$counting_id = 1:dim(berlin_only)[1]
data_end_date = max(berlin_only$emonths)

options(warn=2)
######################
#Classifcations
######################
for(i in 1:length(unique_latlon)){
  printer(i,by = 10000)
  #subset by unique coordinate combination
  outer_dummy = filter(berlin_only, latlon_utm == unique_latlon[i])
  
  #catch coordinates with only one observation
  if(nrow(outer_dummy) == 1){next}
  
  #sort by offering start
  outer_dummy = outer_dummy[order(outer_dummy$amonths),]
  
  #drop non applicable price column as well as observations with missings in key values
  outer_dummy[outer_dummy < 0] = NA
  outer_dummy = drop_na(outer_dummy,wohnflaeche,amonths,emonths,zimmeranzahl,etage,price_var)
  #catch coordinates only incomplete key variables
  if(nrow(outer_dummy) <= 1){next}
  
  #get unique values for living space and drop missings
  unique_wohnflaeche = unique(outer_dummy$wohnflaeche)

  #observation counter. this includes NAs and coordinates with less than 2 observations
  obs_counter$count[obs_counter$state == "Post missing drop"] = obs_counter$count[obs_counter$state == "Post missing drop"] + nrow(outer_dummy)

  for(j in 1:length(unique_wohnflaeche)){
    ##subset by exact/range match in wohnflaeche
    #at least one exact match
    if(sum(outer_dummy$wohnflaeche == unique_wohnflaeche[j]) >= 2){
      inner_dummy = filter(outer_dummy, wohnflaeche == unique_wohnflaeche[j])
      inner_dummy$match_type = "exact"
    } else {
      #range match
      inner_dummy = filter(outer_dummy, rangeChecker(wohnflaeche, unique_wohnflaeche[j], wohnflaeche_offset_factor, "multi"))
      inner_dummy$match_type = "range"
    } 
    
    #cutoff all preceding offerings
    #these cannot be repeated offerings since they were offered before candidate
    inner_dummy = inner_dummy[match(unique_wohnflaeche[j],inner_dummy$wohnflaeche):length(inner_dummy$wohnflaeche),]
    
#Updates
######################   
    ##gen time differences
    inner_dummy = inner_dummy %>% mutate(
      
      #to last date in data
      td_to_end = (as.numeric(data_end_date) - as.numeric(inner_dummy$amonths)),
      #of leading offering
      td_of_lead = (lead(as.numeric(inner_dummy$amonths)) - as.numeric(inner_dummy$emonths))
    )

    #replace last td_of_lead with td_to_end
    inner_dummy$td_of_lead[is.na(inner_dummy$td_of_lead)] = inner_dummy$td_to_end[is.na(inner_dummy$td_of_lead)]

    #drops updates
    inner_dummy = inner_dummy %>% filter(
      .$td_of_lead >= as.numeric((time_offset_factor))
      
    ##intermediary cleanup    
    ) %>% select(
      #drop unused columns
      -td_to_end
    )

    #observation counter
    obs_counter$count[obs_counter$state == "Post update drop"] = obs_counter$count[obs_counter$state == "Post update drop"] + nrow(inner_dummy)

#Repeated
###################### 
    #extract inital offering
    baseline = inner_dummy[1,]
    
    ##gen similarity dummys
    inner_dummy = inner_dummy %>% mutate(

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
        obj_parent = ifelse(repeated_id != 4,baseline$counting_id,NA),
        ##gen price difference to parent
        #this needs to be variable depending on object type
        pd_to_parent = price_var - baseline$price_var,
        ##gen time difference to parent
        td_start_to_parent_end =  (as.numeric(amonths) - as.numeric(baseline$emonths)),
        td_end_to_parent_end =  (as.numeric(emonths) - as.numeric(baseline$emonths))
    )
    
    ##final cleanup        
    final_inner = inner_dummy %>% 
      #drop offerings without parent
      filter(!is.na(obj_parent)) %>%
      mutate(
        changed_ausstattung = case_when(as.numeric(ausstattung) - lag(as.numeric(ausstattung)) > 0 ~ "1", TRUE ~ "0"),
        changed_objektzustand = case_when(as.numeric(objektzustand) - lag(as.numeric(objektzustand)) > 0 ~ "1", TRUE ~ "0"),
        changed_heizungsart = case_when(as.numeric(heizungsart) - lag(as.numeric(heizungsart)) > 0 ~ "1", TRUE ~ "0"),
        changed_einbaukueche = case_when(as.numeric(einbaukueche) - lag(as.numeric(einbaukueche)) > 0 ~ "1", TRUE ~ "0"),
        changed_garten = case_when(as.numeric(garten) - lag(as.numeric(garten)) > 0 ~ "1", TRUE ~ "0"),
        changed_keller = case_when(as.numeric(keller) - lag(as.numeric(keller)) > 0 ~ "1", TRUE ~ "0")
      )
    
    #append to master and remove wohnflaeche subset array
    if(nrow(final_inner)>1){
      final_outer = rbind(final_outer, final_inner)
    }
    #observation counter
    obs_counter$count[obs_counter$state == "Post classification drop"] = obs_counter$count[obs_counter$state == "Post classification drop"] + nrow(final_inner)
    
    rm(inner_dummy)
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

file_name = paste0("repeated_offerings_",Sys.Date(),".dta")
write_dta(final_list,paste0(writepath,file_name))
write.csv2(obs_counter,paste0(writepath,"obs_counter.csv"))
######################
#Cleanup 
######################
rm(list = setdiff(setdiff(ls(),c("readpath","writepath","path")), lsf.str()))
