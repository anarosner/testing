setwd("C:/ALR/GitHub/testing/temp_files/from Ben")
load("gcmData.RData")
load("tempGCMData.RData")

str(both)
both[1:10,]

setwd("C:/ALR/GitHub/testing/temp_files/from Austin/gcm_stuff")
load("met.Rdata")
str(final_met)
dim(final_met)
final_met[1:10,112,]
#max temp, min temp, total precip? 
#year starting?
#seasonal?  spring, summer, fall, winter?  (1,2,3,4, 1 is spring?)



setwd("C:/ALR/GitHub/somepath/basins/west_brook")
met_data<-read.table("met_data.txt")
colnames(met_data) <- c("YEAR","MONTH","DAY","PRCP","TMAX","TMIN","WIND")
met_data[1:10,]
met_data[c(1,nrow(met_data)),]


setwd("C:/ALR/GitHub/testing/somepath2/runs/11")
daily<-read.table("daily_weather.txt",header=T)
daily[1:10,]
monthly<-read.table("h_monthly_weather.txt",header=T)
monthly[1:10,]

setwd("C:/ALR/GitHub/somepath/data/flow_data")
GCMDays <- read.table("Days_For_ABCDE_GCM.txt",header=TRUE)
GCMDays[1:10,]


setwd("C:/ALR/GitHub/testing/r/supporting_scripts")
source("Hargreaves.R") #This generates ET values and may get moved to the met section
met_data<-read.table("met_data.txt")
colnames(met_data) <- c("YEAR","MONTH","DAY","PRCP","TMAX","TMIN","WIND")

# hist_monthly_temp <- aggregate(met_data[,c(1:2,5:6)],FUN=mean,by=list(met_data[,2],met_data[,1]))[,c(1:2,5:6)]
# names(hist_monthly_temp)<-names(met_data[,c(1:2,5:6)])                          #FINAL_HISTORIC_MONTHLY1
# hist_monthly_temp[1:10,]
# hist_monthly_precip <- aggregate(met_data[,c(1:2,4)],FUN=sum,by=list(met_data[,2], met_data[,1]))[,c(1,2,5)]  #FINAL_HISTORIC_MONTHLY2
# names(hist_monthly_precip)<-names(met_data[,c(1:2,4)])
# hist_monthly_precip[1:10,]
# 
# hist_monthly_temp$TAVG <- (hist_monthly_temp[,"TMAX"]+hist_monthly_temp[,"TMIN"])/2
# 
# # HIST_TEMP_MONTH <- aggregate(met_data[,c(1:2,5:6)],FUN=mean,by=list(met_data[,2]))[4:5]
# # FINAL_HISTORIC_MONTH1 <- (HIST_TEMP_MONTH$TMAX+HIST_TEMP_MONTH$TMIN)/2
# # FINAL_HISTORIC_MONTH2 <- aggregate(met_data[,c(1:2,4)],FUN=sum,by=list(met_data[,2]))[,4]/62 #62 years in historic timeseries
# 
# SDIF <- FINAL_STOCHASTIC_MONTHLY1[,3]-FINAL_STOCHASTIC_MONTHLY1[,4]
# hist_monthly_temp$TDIF <- hist_monthly_temp[,"TMAX"]-hist_monthly_temp[,"TMIN"]
# hist_monthly_temp$HIST_ET <- Hargreaves(hist_monthly_temp$HTAVG, hist_monthly_temp$HDIF, BasinLat, GCMDays$Jday[1:744], GCMDays$Days[1:744])
# 
# FINAL_HISTORIC_MONTHLY <- cbind(hist_monthly_temp[,2],
#                                 hist_monthly_temp[,1],
#                                 round(hist_monthly_precip,2),
#                                 round(hist_monthly_temp[,3:4],2),
#                                 round(HTAVG,2),
#                                 round(HIST_ET,2),
#                                 round(HDIF,2))
# 
# colnames(FINAL_HISTORIC_MONTHLY) <- c("YEAR","MONTH","PRCP","TMAX","TMIN","TAVG","ET","DIF")


BasinLat<-42.44469112

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

FINAL_HISTORIC_MONTHLY <- cbind(hist_monthly_temp[,2],
                               hist_monthly_temp[,1],
                                round(hist_monthly_precip,2),
                                round(hist_monthly_temp[,3:4],2),
                                round(HTAVG,2),
                                round(HIST_ET,2),
                                round(HDIF,2))

colnames(FINAL_HISTORIC_MONTHLY) <- c("YEAR","MONTH","PRCP","TMAX","TMIN","TAVG","ET","DIF")
