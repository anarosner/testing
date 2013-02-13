library(rjson)

{
if (!exists("testingargs")) {
   #args consists of single json object which contains run_dir (exact path for this run), basin_dir, data_dir
   args<-fromJSON( commandArgs(trailingOnly = TRUE) )

}
else {

   #testing, not from command line and/or not called from model scripts
   print("testing, not from command line")
   print(testingargs)
   args<-testingargs
}
}

#load param functions
source("./supporting_scripts/param_functions.R")

this_dir <- getwd()
run_dir<-args$run_dir
   directory<-args$run_dir #to comply with old code
basin_dir<-args$basin_dir
data_dir<-args$data_dir

setwd(run_dir)
param<-fromJSON(file="settings.json")

setwd(this_dir)
config<-fromJSON(file=paste0(param$scriptName,".json"))

preceding<-param$preceding
basin_id<-parse.param("basin_id")
    
    # #Westbrook Example (0)
    # # Any Basin (1)
    # {
    # 	if (parse.param("basin_id")=="west_brook")
    # 	   run_option <-0
    # 	else
    # 	   run_option <-1
    # }



    # attach(param$preceding)
    # # 
    # #parse all the preceding run ids
    # if (!is.null(param$preceding)) {
    # #    pre<-param$preceding
    # #    length(pre)
    #    pre<-data.frame(dummy=c(1))
    #    for ( i in 1:length(param$preceding) ) {
    #       pre[i] <- param$preceding[i]     
    # #       preceding[paste0(names(pre[i]),"_dir")]<-paste0(basedir,"/runs/",ids$userid,"/",pre[i][[1]])      
    #    }
    #    preceding<-pre[,-1]
    # }
    

