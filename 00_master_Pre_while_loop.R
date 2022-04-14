######################
#Package Install Function
######################
packages_install = function(library_string){
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(library_string, character.only = TRUE)    
  print("Packages fully installed and loaded")
}
Test
######################
#Packages
######################
library_string = c("here", # creating dynamic paths based on script location
                   "haven", # reading/writing of dta files
                   "tidyverse" # data manipulation/wrangeling
)
packages_install(library_string)
######################
#Paths
######################
path = paste0(here(),"/")
readpath = "M:/_FDZ/RWI-GEO/RWI-GEO-RED/daten/On-site/v6/"
writepath = paste0(path,"Data/Output/")

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
#Prep
######################
wk_data = read_dta(paste0(readpath,"WK_allVersionsLabels.dta"))
names(wk_data)
#shrink data to necessary
berlin_only = 
  wk_data %>%
  filter(
    # berlin
    blid == 11, 
    # remove missings
    as.character(lat_utm) != "-9",
    as.character(lon_utm) != "-9",
    na.rm = TRUE
    ) %>%
  select(
    obid,
    uniqueID_gen,
    mietekalt,
    mietewarm,
    kaufpreis,
    wohnflaeche,
    etage,
    zimmeranzahl,
    dupID_gen,
    ajahr,
    amonat,
    ejahr,
    emonat,
    lat_utm,
    lon_utm,
    lat_gps,
    lon_gps
  ) %>%
  #new var
  mutate(
    latlon_utm = paste0(lat_utm,lon_utm),
    #monat + 12 to avoid sorting issues in same year
    ajahrmonat = paste0(ajahr,amonat+12),
    ejahrmonat = paste0(ejahr,emonat+12)
  )
rm(wk_data)

######################
#Declarations
######################
#vars are allowed to deviate up to this factor in both directions
wohnflaeche_offset_factor = 0.1
etage_offset_factor = 0
zimmeranzahl_offset_factor = 1
time_offset_factor = 6

final_list = c()
######################
#Section
######################
unique_latlon = unique(berlin_only$latlon_utm)
berlin_only$counting_id = 1:dim(berlin_only)[1]
#length(unique_latlon)

for(i in 1:100){
  #subset by unique coordination combination
  outer_dummy = filter(berlin_only, latlon_utm == unique_latlon[i])
  outer_dummy = outer_dummy[order(outer_dummy$ajahrmonat),]
  unique_wohnflaeche = unique(outer_dummy$wohnflaeche)
  
  printer(i,by=10)

  j = 1
  while(j <= length(unique_wohnflaeche)){
    
    #at least one exact match
    if(sum(outer_dummy$wohnflaeche == unique_wohnflaeche[j]) >= 2){
      inner_dummy = filter(outer_dummy, wohnflaeche == unique_wohnflaeche[j])
      inner_dummy$match_type = "exact"
        
    } else {
      #range match
      inner_dummy = filter(outer_dummy, rangeChecker(wohnflaeche, unique_wohnflaeche[j], wohnflaeche_offset_factor, "multi"))
      inner_dummy$match_type = "range"
    } 
    
    #extract inital offering
    baseline = inner_dummy[1,]
    
    #rangeChecker
    inner_dummy = inner_dummy %>% mutate(
      zimmeranzahl_similar = rangeChecker(inner_dummy$zimmeranzahl, baseline$zimmeranzahl, zimmeranzahl_offset_factor, "add"),
      etage_similar = rangeChecker(inner_dummy$etage, baseline$etage, etage_offset_factor, "add"),
      time_diff = (as.numeric(inner_dummy$ajahrmonat) - as.numeric(baseline$ejahrmonat))
    )
    
    #gen repeated id based on match type
    inner_dummy = inner_dummy %>% mutate(repeated_id = case_when(
      
      #is baseline
      .$counting_id == baseline$counting_id ~ "0",
      
      #both match, true repeated offering
      .$etage == baseline$etage & .$zimmeranzahl == baseline$zimmeranzahl ~ "1",
      
      #one matches, one similar, half-true repeated offering
      .$etage == baseline$etage & .$zimmeranzahl_similar ~ "2",
      .$etage_similar & .$zimmeranzahl == baseline$zimmeranzahl ~ "2",
      
      #both are similar, similar repeated offering
      .$etage_similar & .$zimmeranzahl_similar ~ "3",
      
      #no matches
      TRUE ~ "4"
    )
    ) %>% mutate(update_id = case_when(
      #same obj
      .$repeated_id == 0 ~ "0",
      
      #more than specified
      .$time_diff >= as.numeric((time_offset_factor)) ~ "1", 
      
      #less than specified
      TRUE ~ "2" 
    )
    )  %>% mutate(obj_parent = ifelse(repeated_id != 4,baseline$counting_id,NA))
    
    #filter out repeated offerings that are only updates
    #gets stuck in loop here somewhere
    if(sum("2" == inner_dummy$update_id & "1" == inner_dummy$repeated_id) >= 1){
      if("1" %in% inner_dummy$update_id){
        
        earliest_sale = match("1", inner_dummy$update_id)
        last_update_before_sale = inner_dummy$counting_id[which.max(inner_dummy$counting_id[1:earliest_sale - 1])]
        
        remove_counting_ids = inner_dummy$counting_id[!(inner_dummy$counting_id == last_update_before_sale) & inner_dummy$update_id %in% c("0","2")]
      
        outer_dummy = outer_dummy %>% filter(!counting_id %in% remove_counting_ids)
        next
        
      } else {
        
        #this may be useless. if there are no sell-events there can be no repeated offerings
        
        #find last update
        latest_update = which.max(inner_dummy$counting_id)
        preceding_counting_ids = inner_dummy$counting_id[1:latest_update - 1]
        
        #remove all preceding updates, set last one as new baseline and redo loop step
        outer_dummy = outer_dummy %>% filter(!counting_id %in% preceding_counting_ids)
        next
      }
    }
    
    final_inner = inner_dummy %>% filter(!is.na(obj_parent) & !update_id == "2") %>% select(wohnflaeche,etage,zimmeranzahl,counting_id,repeated_id,update_id,obj_parent,time_diff,latlon_utm)
    final_list = rbind(final_list, final_inner)
    rm(inner_dummy)
    j = j + 1
  }
  rm(outer_dummy)
}




#5824292.09293933793584.658496123
#1.5 to 1 rooms
######################
#Cleanup 
######################
rm(list = setdiff(setdiff(ls(),c("readpath","writepath","path")), lsf.str()))

