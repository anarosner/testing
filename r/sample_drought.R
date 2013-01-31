#!/usr/bin/env Rscript
print("starting historic baseline climate script")

this_dir<-getwd()
source("./supporting_scripts/param_startup.R")
source("./supporting_scripts/plot_functions.R")
source("./supporting_scripts/Hargreaves.R") #This generates ET values and may get moved to the met section

GCMDays <- read.table(paste0(args$data_dir,"/flow_data/Days_For_ABCDE_GCM.txt"),header=TRUE)


#parse user input params
change_precip_mean <- parse.param("precip_mean_y1")/100
# change_temp_mean <- parse.param("temp_mean_y1")
percentile<-parse.param("percentile")/100

#load data
basin_id <- parse.param("basin_id")
basin_param <- fromJSON(file=paste0(basin_dir,"/",basin_id,"/param.json"))


precipannual<-read.csv(paste0(basin_dir,"/",basin_id,"/precipannualh.csv"),header=T)
precipannual<-read.csv(paste0(basin_dir,"/",basin_id,"/precipannual",change_precip_mean,".csv"),header=T)
precip<-read.csv(paste0(basin_dir,"/",basin_id,"/precip",change_precip_mean,".csv"),header=T)
# tempannual<-read.csv(paste0(basin_dir,"/",basin_id,"/tempannualh.csv"),header=T)

sort<-precipannual[order(precipannual$precip.in),]
value<-sort[round(nrow(sort)*percentile)-1,"precip.in"]
value

i<-ceiling(runif(1,min=0,max=nrow(precipannual)))
flag<-0
while (flag==0) {
#    print(i)
#    print(precipannual$precip.in[i])
   {
   if ( precipannual$precip.in>=floor(value-1) & precipannual$precip.in<=ceiling(value+1) )
      flag<-1
   else { 
      if (i+1>nrow(precipannual) )
         i<-1
      else
         i<-i+1
      }
   }
}
print(i)
print(precipannual$precip.in[i])


precip<-read.csv(paste0(basin_dir,"/",basin_id,"/precipannual.csv"),header=T)

