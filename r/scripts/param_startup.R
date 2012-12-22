library(rjson)

{
if (!exists("testingid")) {
   #args consists of single json object which contains userid and runid
   ids<-fromJSON( commandArgs(trailingOnly = TRUE) )
   #these ids are used to find correct directory for the run, which contains param.json, which has all remaining parameters
   print(basedir)
   print(ids)
}
else {

   #testing, not from command line and/or not called from model scripts
   print("testing, not from command line")
   print(testingid)
   setwd("../..")
   basedir<-getwd()
   ids<-list(userid="user1234",runid=testingid)   # ##testing
}
}

#load param functions
setwd(paste0(basedir,"/r/scripts"))
source("param_functions.R")


directory<-paste0(basedir,"/runs/",ids$userid,"/",ids$runid)
setwd(directory)
param<-fromJSON(file="param.json")

setwd(paste0(basedir,"/r/models"))
config<-fromJSON(file=paste0(param$rscript,".json"))


#Westbrook Example (0)
# Any Basin (1)
{
	if (parse.param("basinid")=="west_brook")
	   run_option <-0
	else
	   run_option <-1
}


#parse all the preceding run ids
if (!is.null(param$preceding)) {
   pre<-param$preceding
   length(pre)
   preceding<-data.frame(dummy=c(1))
   for ( i in 1:length(pre) ) {
      pre[i]      
      preceding[paste0(names(pre[i]),"_dir")]<-paste0(basedir,"/runs/",ids$userid,"/",pre[i][[1]])      
   }
   #preceding<-preceding[,-1]
}


#set wd to where it was before this script was called
setwd(paste0(basedir,"/r/scripts"))
