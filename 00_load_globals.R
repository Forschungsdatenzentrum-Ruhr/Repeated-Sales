######################
#Explanation
######################
load_globals = function(){

  ######################
  #Paths
  ######################
  
  writepath = paste0(path,"Output/")

  
  
  
  
  ######################
  #Logging
  ######################
  # create log file with current date
  #log_name = paste0("log_",Sys.Date(),".txt")
  #log_file = paste0(writepath,log_name)
  
  # write logs to both console and file
  #log_appender(appender_tee(log_file))
  
  ######################
  #parallel
  ######################
  # detect number of cores present and assign as workers
  #numCores = detectCores()
  #registerDoParallel(cores = numCores)
  
}