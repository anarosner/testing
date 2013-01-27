#!/usr/bin/env Rscript
print("starting historic baseline climate script")

this_dir<-getwd()
source("./supporting_scripts/param_startup.R")
source("./supporting_scripts/plot_functions.R")


met_data<-read.table(paste0(basin_dir,"/",basin_id,"/met_data.txt"),header=FALSE)
colnames(met_data) <- c("YEAR","MONTH","DAY","PRCP","TMAX","TMIN","WIND")

setwd(paste0(this_dir,"/supporting_scripts"))
source("Hargreaves.R") #This generates ET values and may get moved to the met section
GCMDays <- read.table(paste0(args$data_dir,"/flow_data/Days_For_ABCDE_GCM.txt"),header=TRUE)


hist_monthly <- aggregate(met_data[,c(1:2,4)],FUN=sum,by=list(met_data[,2], met_data[,1]))[,c(1,2,5)]  #FINAL_HISTORIC_MONTHLY2
names(hist_monthly)<-names(met_data[,c(1:2,4)])
hist_monthly[,c("TMAX","TMIN")] <- aggregate(met_data[,c(1:2,5:6)],FUN=mean,by=list(met_data[,2],met_data[,1]))[,c(5:6)]

hist_monthly[1:10,]

hist_monthly$TAVG <- (hist_monthly[,"TMAX"]+hist_monthly[,"TMIN"])/2

# HIST_TEMP_MONTH <- aggregate(met_data[,c(1:2,5:6)],FUN=mean,by=list(met_data[,2]))[4:5]
# FINAL_HISTORIC_MONTH1 <- (HIST_TEMP_MONTH$TMAX+HIST_TEMP_MONTH$TMIN)/2
# FINAL_HISTORIC_MONTH2 <- aggregate(met_data[,c(1:2,4)],FUN=sum,by=list(met_data[,2]))[,4]/62 #62 years in historic timeseries

# SDIF <- FINAL_STOCHASTIC_MONTHLY1[,3]-FINAL_STOCHASTIC_MONTHLY1[,4]
hist_monthly$HIST_ET<-0 #to preserve order
hist_monthly$DIF <- hist_monthly[,"TMAX"]-hist_monthly[,"TMIN"]
hist_monthly$ET <- Hargreaves(hist_monthly$TAVG, hist_monthly$TDIF, BasinLat, GCMDays$Jday[1:744], GCMDays$Days[1:744])
#HTAVG,HDIF,BasinLat,GCMDays$Jday[1:744],GCMDays$Days[1:744]
hist_monthly[,c(3:7)]<-round(hist_monthly[,c(3:7)],2)