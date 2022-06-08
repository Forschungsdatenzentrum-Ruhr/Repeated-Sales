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
                   "fst"
)
packages_install(library_string)
######################
#Paths
######################
path = paste0(here(),"/")
readpath = "M:/_FDZ/RWI-GEO/RWI-GEO-RED/daten/On-site/v6/"
writepath = paste0(path,"Output/")

wm_data = read_dta(paste0(writepath,"repeated_offerings_2022-05-20.dta"))
#write_fst(wm_data,paste0(writepath,"repeated_offerings_2022-05-20.fst"))
#test = wm_data[1:100,]

#wk_data = read_fst(paste0(writepath,"repeated_offerings_2022-05-20.dta"))
wk_data = read_dta(paste0(writepath,"repeated_offerings_2022-05-20.dta"))
#table(wk_data$repeated_id)

#length(unique(wk_data$latlon_utm))
#table(table(wk_data$obj_parent))

#test = filter(wk_data, !repeated_id == "3")
#table(table(test$obj_parent))
