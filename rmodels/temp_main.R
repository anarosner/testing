library(rjson)

#load param/config functions, currently stored in the rmodels folder
setwd("/home/ana/testing_dir_structure/rmodels")
source("param.functions.R")

#load param for this specific run
#eventually, info userid and runid will come from the webapp (javascript, etc) code 
#current file structure is base/runs/userid/runid
setwd("/home/ana/testing_dir_structure/runs/1234/1009")
param<-fromJSON(file="settings.json")

#get the rscript from the param file.  then load the appropriate config file
setwd("/home/ana/testing_dir_structure/rmodels")
config<-fromJSON(file=paste0(param$rscript,".json"))
source(paste0(param$rscript,".R"))
