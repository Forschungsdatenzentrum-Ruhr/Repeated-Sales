make_var <- function(data_type) {
  if (data_type == "WK") {
    depVar <- "ln_flatprice_sqm"
    indepVar <- c(
      
      # raw
      "balkon",
      "garten",
      "einbaukueche",
      "gaestewc",
      "aufzug",
      "keller",
      "betreut",
      "ausstattung",
      "zimmeranzahl",
      
      # mutated
      "declared_wohngeld",
      "baujahr_cat",
      "first_occupancy",
      "num_floors",
      "floors_cat"
    )
    fixed_effects <- c(
      "gid2019", 
      "ejahr"
    )
    
    binary_names <- c(
      "balkon",
      "garten",
      "einbaukueche", 
      "gaestewc", 
      "aufzug", 
      "keller", 
      "betreut", 
      "ausstattung", 
      "declared_wohngeld", 
      "baujahr_cat", 
      "first_occupancy", 
      "num_floors", 
      "floors_cat"
    )
    cont_names <- c("zimmeranzahl")
  } else if (data_type == "WM") {
    depVar <- "ln_rent_sqm"
    indepVar <- c(
      
      # raw
      "balkon",
      "garten",
      "einbaukueche",
      "gaestewc",
      "aufzug", # this isnt in REDX; why?
      "keller",
      "ausstattung",
      "zimmeranzahl",
      
      # mutated
      "baujahr_cat",
      "first_occupancy"
    )
    fixed_effects <- c("gid2019", "ejahr")
    
    binary_names <- c(
      "balkon",
      "garten",
      "einbaukueche", 
      "gaestewc", 
      "aufzug", 
      "keller", 
      "ausstattung",
      "baujahr_cat", 
      "first_occupancy", 
    )
    cont_names <- c("zimmeranzahl")
    
    
    
  } else if (data_type == "HK") {
    depVar <- "ln_houseprice_sqm"
    
    indepVar <- c(
      
      # raw
      "gaestewc",
      "einliegerwohnung",
      "ausstattung",
      "zimmeranzahl",
      
      # mutated
      "baujahr_cat",
      "first_occupancy",
      "plotarea_cat",
      "type_cat"
    )
    fixed_effects <- c(
      "gid2019", 
      "ejahr")
    
    binary_names <- c(
      # raw
      "gaestewc",
      "einliegerwohnung",
      "ausstattung",
      
      # mutated
      "baujahr_cat",
      "first_occupancy",
      "plotarea_cat",
      "type_cat" 
    )
    cont_names <- c("zimmeranzahl")
    
  }
  list_var <- list(depVar = depVar, indepVar = indepVar, fixed_effects = fixed_effects, binary_names = binary_names, cont_names = cont_names)
  return(list_var)
}