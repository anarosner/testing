#!/usr/bin/env Rscript
print("starting historic baseline climate script")

this_dir<-getwd()
source("./supporting_scripts/param_startup.R")
source("./supporting_scripts/plot_functions.R")
source("./supporting_scripts/Hargreaves.R") #This generates ET values and may get moved to the met section

GCMDays <- read.table(paste0(args$data_dir,"/flow_data/Days_For_ABCDE_GCM.txt"),header=TRUE)

#load data
basin_id <- parse.param("basin_id")
basin_param <- fromJSON(file=paste0(basin_dir,"/",basin_id,"/param.json"))


h_daily<-read.table(paste0(basin_dir,"/",basin_id,"/met_data.txt"),header=FALSE)
colnames(h_daily) <- c("YEAR","MONTH","DAY","PRCP","TMAX","TMIN","WIND")

y1<-h_daily[1,"YEAR"]
yn<-h_daily[nrow(h_daily),"YEAR"]
n_years<-yn-y1


#aggregate to monthly
h_monthly <- aggregate(h_daily[,c(1:2,4)],FUN=sum,by=list(h_daily[,2], h_daily[,1]))[,c(2,1,5)]  
names(h_monthly)<-names(h_daily[,c(1:2,4)])
h_monthly[,c("TMAX","TMIN")] <- aggregate(h_daily[,c(1:2,5:6)],FUN=mean,by=list(h_daily[,2],h_daily[,1]))[,c(5:6)]

#calculated additional values needed for subsequent models
#calculate t avg and t diff
h_monthly$TAVG <- (h_monthly[,"TMAX"]+h_monthly[,"TMIN"])/2
h_monthly$ET<-0 #to preserve order
h_monthly$DIF <- h_monthly[,"TMAX"]-h_monthly[,"TMIN"]
h_monthly$ET <- Hargreaves(h_monthly$TAVG, h_monthly$DIF, basin_param$lat, GCMDays$Jday[1:744], GCMDays$Days[1:744])
#round
h_monthly[,c(3:7)]<-round(h_monthly[,c(3:7)],2) #round all output
# print("h monthly")
# 
# 
# h_monthly[1:15,]
# h_daily[1:15,]

setwd(run_dir)
write.table(h_monthly,"hist_monthly_weather.txt",row.names=FALSE,quote=FALSE,col.names=TRUE) # this is the mean monthly timeseries
write.csv(h_monthly,"hist_monthly_weather.csv",row.names=FALSE,quote=FALSE)

write.table(h_daily,"daily_weather.txt",row.names=FALSE,quote=FALSE) #This is the daily timeseries
write.csv(h_daily,"daily_weather.csv",row.names=FALSE,quote=FALSE)

h.clim<-h_monthly
plot.thumbnail(type="historic")
# plot.thumbnail(type="airtemp")

