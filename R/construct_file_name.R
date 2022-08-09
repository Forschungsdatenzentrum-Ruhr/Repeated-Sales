construct_file_name = function(data_version = "v6",type = c("Wk","Wm")){
  
  path = paste0("M:/_FDZ/RWI-GEO/RWI-GEO-RED/daten/On-site","/",data_version, "/")
  
  file_name = paste0(path,type,"_allVersionsLabels.dta")
  
  
  return(file_name)
}
    