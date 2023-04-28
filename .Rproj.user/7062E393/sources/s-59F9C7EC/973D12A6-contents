read_federal_state <- function(filename = NA, federal_state_id = NA) {
  #' @title Reading of RED and subsetting to federal_state
  #'
  #' @description reads in data, subsets to requested federal state and
  #' does preliminary cleanup/mutation
  #' 
  #' @param filename file name of data to be used
  #' @param federal_state_id static id of federal state. Has to be 1:16
  #'
  #' @note
  #'
  #' @return A unlabeled data table for specified federal state
  #' @author Thorben Wiebe 
  #----------------------------------------------

  # declare necessary non-missing entries in the data
  # this might be useful to specify in _target
  var_of_interest <- c("wohnflaeche", "amonths", "emonths", "zimmeranzahl", "etage", "price_var")

  ### read inital data
  bl <- read_dta(filename)|> 
    ## subset
    filter(
      # to given federal state
      blid == federal_state_id,

      # drop missing coordinates
      !lat_utm < 0 | !lon_utm < 0
    ) |>
    # drop text variables
    select(
      -freiab,
      -mietekaution,
      -courtage
    ) |>
    ## gen static variables
    mutate(
      # coordinate combination
      latlon_utm = paste0(lat_utm, lon_utm),

      # transform years into months and add running years months
      amonths = ajahr * 12 + amonat,
      emonths = ejahr * 12 + emonat,

      # merge price and rent into one variable
      price_var = pmax(mietekalt, kaufpreis),

      # create unique id for entire data
      counting_id = 1:n(),

      # get enddate of date
      data_end_date = max(emonths)
    )

  # recode and drop relevant missings
  bl[bl < 0] <- NA
  bl %<>% drop_na(var_of_interest) |> zap_labels(.)

  return(bl)
}
