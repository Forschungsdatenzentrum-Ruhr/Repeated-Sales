read_RED <- function(RED_file_name = NA, var_of_interest = NA) {
  #' @title WIP
  #'
  #' @description WIP
  #' @param WIP
  #' @param WIP
  #' @note 
  #'
  #' @return WIP
  #' @author Thorben Wiebe 
  #----------------------------------------------
  
  # read stata file and remove labels
  RED = haven::read_dta(RED_file_name) |> 
    haven::zap_labels() |> 
    data.table::setDT()[
    # drop missing coordinates
    lat_utm > 0 | lon_utm > 0,
    # keep only relevant variables
    ..var_of_interest
    ][
      ,
      ## mutations
      ":="(
        # combine coordinates
        latlon_utm = paste0(lat_utm, lon_utm),
        
        # transform years into months and add running years months
        amonths = ajahr * 12 + amonat,
        emonths = ejahr * 12 + emonat,
        # combine price and rent into one variable
        price_var = pmax(mietekalt, kaufpreis),
        
        # create unique id for entire data
        counting_id = 1:.N,
        
        # cleanup
        ajahr = NULL,
        amonat = NULL,
        ejahr = NULL,
        emonat = NULL,
        mietekalt = NULL,
        kaufpreis = NULL
        
        # TODO
        # get enddate of date
        # data_end_date = max(emonths)
        
      ) 
    ] 
    setkey(RED,blid,latlon_utm)
    
  # recode negative values to NA
  # little more convoluted but way faster variant of RED[RED < 0] <- NA 
  for(col in names(RED)){
    set(RED, i=which(RED[[col]]<0), j = col, value = NA)
  }
  # drop NAs
  na.omit(RED)
  
  
  
  
  return(bl)
}
  