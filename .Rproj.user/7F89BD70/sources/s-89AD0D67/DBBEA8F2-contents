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
                   "tidyverse" # data manipulation/wrangeling
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


missings = c(-5:-11)
final_outer = c()
final_inner = c()
final_list = c()

######################
#Prep
######################
wk_data = read_dta(paste0(readpath,"WK_allVersionsLabels.dta"))

#shrink data to necessary
berlin_only =
  wk_data %>%
  filter(
    # berlin only
    blid == 11
  ) %>%
  select(
    mietekalt,
    kaufpreis,
    wohnflaeche,
    etage,
    zimmeranzahl,
    ajahr,
    amonat,
    ejahr,
    emonat,
    lat_utm,
    lon_utm
  ) %>%
  #new var
  mutate(
    #coordinate combination
    latlon_utm = paste0(lat_utm,lon_utm),
    #transform years into months and add running years months
    amonths = ajahr * 12 + amonat,
    emonths = ejahr * 12 + emonat 
  ) %>%
  #drop unused columns
  select(
    -ajahr,
    -amonat,
    -ejahr,
    -emonat,
    -lat_utm,
    -lon_utm
  )
rm(wk_data)

unique_latlon = unique(berlin_only$latlon_utm)
berlin_only$counting_id = 1:dim(berlin_only)[1]
data_end_date = max(berlin_only$emonths)

######################
#Classifcations
######################
#length(unique_latlon)
for(i in 1:100){
  printer(i,by = 1)
  #subset by unique coordinate combination
  outer_dummy = filter(berlin_only, latlon_utm == unique_latlon[i])
  
  #catch coordinates with only one observation
  if(nrow(outer_dummy)==1){next}
  
  #sort by offering start
  outer_dummy = outer_dummy[order(outer_dummy$amonths),]
  
  #drop non applicable price column as well as observations with missings in key values
  miss_matrix = as.data.frame(apply(outer_dummy, 2 ,function(x) as.integer(x %in% missings)))
  outer_dummy = outer_dummy[rowSums(miss_matrix) == 1,]#!colSums(miss_matrix) == nrow(miss_matrix)
  
  #catch coordinates only incomplete key variables
  if(nrow(outer_dummy)==0){next}
  
  #get unique values for living space and drop missings
  unique_wohnflaeche = unique(outer_dummy$wohnflaeche)
  
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
        kp_pd_to_parent = kaufpreis - baseline$kaufpreis,
        rent_pd_to_parent = mietekalt - baseline$mietekalt,
        ##gen time difference to parent
        td_to_parent =  (as.numeric(amonths) - as.numeric(baseline$emonths))
    )
    ##final cleanup        
    final_inner = inner_dummy %>% 
      #drop offerings without parent
      filter(!is.na(obj_parent)) %>%
      #drop all colums but those of interest
      select(counting_id,obj_parent,repeated_id,kaufpreis,kp_pd_to_parent,mietekalt,rent_pd_to_parent,amonths,emonths,td_to_parent,match_type,wohnflaeche,etage,zimmeranzahl,latlon_utm)
    
    #append to master and remove wohnflaeche subset array
    if(nrow(final_inner)>1){
      final_outer = rbind(final_outer, final_inner)
    }
    rm(inner_dummy)
  }
  #remove unique coordination combination subset array
  rm(outer_dummy)
}
#count occurences of parent objects
final_outer = distinct(final_outer)
count_parents = count(final_outer,obj_parent)
keep_parents = count_parents[1][count_parents[2] > 1]
#drop if object is only its own parent
final_list = final_outer %>% filter(obj_parent %in% keep_parents) %>% arrange(obj_parent,counting_id)

######################
#Cleanup 
######################
#rm(list = setdiff(setdiff(ls(),c("readpath","writepath","path")), lsf.str()))

