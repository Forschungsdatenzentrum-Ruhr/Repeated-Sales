classification = function(unique_wohnflaeche,outer_dummy){
  
  #vars are allowed to deviate up to this factor in both directions
  wohnflaeche_offset_factor = 0.1
  etage_offset_factor = 99
  zimmeranzahl_offset_factor = 0.5
  time_offset_factor = 6
  
  final_inner = foreach(wohn = unique_wohnflaeche, .combine = "rbind", .inorder = F, .packages = c("tidyverse","magrittr")) %do%{
    
    #subset by range match in wohnflaeche
    inner_dummy = filter(outer_dummy, rangeChecker(wohnflaeche, wohn, wohnflaeche_offset_factor, "multi"))
    
    #cutoff all preceding offerings
    #these cannot be repeated offerings since they were offered before candidate
    inner_dummy = inner_dummy[match(wohn,inner_dummy$wohnflaeche):length(inner_dummy$wohnflaeche),]
  
    #gen time difference variables
    inner_dummy %<>% mutate(
      
      #to last date in data
      td_to_end = (as.numeric(data_end_date) - as.numeric(inner_dummy$amonths)),
      
      #of leading offering
      td_of_lead = (lead(as.numeric(inner_dummy$amonths)) - as.numeric(inner_dummy$emonths)),
      
      #replace last td_of_lead with td_to_end
      td_of_lead = replace_na(td_of_lead, td_to_end[is.na(td_of_lead)])
      
    #Updates
    ######################  
    ) %>% filter(
     
    #drops updates
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
        .$etage == baseline$etage & .$zimmeranzahl_similar ~ "1",
        
        #both are similar, resembling repeated offering
        .$etage_similar & .$zimmeranzahl_similar ~ "3",
        
        #no matches
        TRUE ~ "4"
      ),
      ##gen proposed object parent
      obj_parent = ifelse(repeated_id != 4,baseline$counting_id, NA),
      
      ##gen price difference to parent
      pd_to_parent = price_var - baseline$price_var,
      
      ##gen time difference to parent
      td_start_to_parent_end =  (as.numeric(amonths) - as.numeric(baseline$emonths)),
      td_end_to_parent_end =  (as.numeric(emonths) - as.numeric(baseline$emonths))
    ) %>% 
      
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
    #return classified dataframe to loop
    inner_dummy
  }
  return(final_inner)
}



