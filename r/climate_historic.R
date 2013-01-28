#!/usr/bin/env Rscript
print("starting historic baseline climate script")

this_dir<-getwd()
source("./supporting_scripts/param_startup.R")
source("./supporting_scripts/plot_functions.R")
source("./supporting_scripts/Hargreaves.R") #This generates ET values and may get moved to the met section

GCMDays <- read.table(paste0(args$data_dir,"/flow_data/Days_For_ABCDE_GCM.txt"),header=TRUE)


#parse user input params
change_precip_mean_y1 <- parse.param("precip_mean_y1")/100
change_precip_mean_yn <- parse.param("precip_mean_yn")/100 
change_precip_var_y1 <- parse.param("precip_var_y1")/100 
change_precip_var_yn <- parse.param("precip_var_yn")/100 
change_temp_mean_y1 <- parse.param("temp_mean_y1")
change_temp_mean_yn <- parse.param("temp_mean_yn")


#load data
basin_id <- parse.param("basin_id")
basin_param <- fromJSON(file=paste0(basin_dir,"/",basin_id,"/param.json"))


h_daily<-read.table(paste0(basin_dir,"/",basin_id,"/met_data.txt"),header=FALSE)
colnames(h_daily) <- c("YEAR","MONTH","DAY","PRCP","TMAX","TMIN","WIND")

y1<-h_daily[1,"YEAR"]
yn<-h_daily[nrow(h_daily),"YEAR"]
n_years<-yn-y1

#baseline shift
precip_mean<-mean(h_daily$PRCP)
precip_mean_slope<-(change_precip_mean_yn - change_precip_mean_y1)*precip_mean/nrow(h_daily)
precip_var<-var(h_daily$PRCP)
precip_var_slope<-(change_precip_var_yn - change_precip_var_y1)*precip_var/nrow(h_daily)
tmin_mean<-mean(h_daily$TMIN)
tmax_mean<-mean(h_daily$TMAX)
t_mean_slope<-(change_temp_mean_yn-change_temp_mean_y1)/nrow(h_daily)


s_daily<-h_daily[,c("YEAR","MONTH","DAY")]
s_daily[,c("PRCP", "TMAX", "TMIN")]<-0
s_daily$WIND<-h_daily$WIND
s_daily$precip_mult<-1+(change_precip_mean_y1+as.numeric(row.names(s_daily))*precip_mean_slope)
s_daily$PRCP<-h_daily$PRCP*s_daily$precip_mult
s_daily$precip_mean<- precip_mean*(1+change_precip_mean_y1)+as.numeric(row.names(s_daily))*precip_mean_slope
s_daily$precip_var<- precip_var*(1+change_precip_var_y1)+as.numeric(row.names(s_daily))*precip_var_slope
# s_daily$PRCP<-s_daily$precip_mean + (h_daily$PRCP-precip_mean)*sqrt(s_daily$precip_var)/sqrt(precip_var)
# s_daily$PRCP<-s_daily$precip_mean + s_daily$precip_var/precip_var*(h_daily$PRCP-precip_mean)
# s_daily$PRCP<-s_daily$precip_mean + s_daily$precip_var/precip_var*(h_daily$PRCP-precip_mean)
# s_daily$PRCP[s_daily$PRCP<0]<-0

s_daily$temp_shift<-change_temp_mean_y1+as.numeric(row.names(s_daily))*t_mean_slope
s_daily$TMIN<- h_daily$TMIN+s_daily$temp_shift
s_daily$TMAX<- h_daily$TMAX+s_daily$temp_shift

# (as.numeric(row.names(s_daily))*t_mean_slope)[1]

print("s daily")
print(paste("precip mean",mean(s_daily$PRCP[s_daily$YEAR %in% c(2000:2010)])))
print(paste(mean(s_daily$PRCP[h_daily$YEAR %in% c(2000:2010)]),mean(h_daily$PRCP[s_daily$YEAR %in% c(2000:2010)])*(1+change_precip_mean_yn)))
print(paste("precip var",var(s_daily$PRCP[s_daily$YEAR %in% c(2000:2010)])))
print(paste(var(s_daily$PRCP[h_daily$YEAR %in% c(2000:2010)]),var(s_daily$PRCP[h_daily$YEAR %in% c(2000:2010)])*(1+change_precip_var_yn)))
print(paste("tmax mean",mean(s_daily$TMAX[s_daily$YEAR %in% c(2000:2010)])))
print(paste(mean(s_daily$TMAX[h_daily$YEAR %in% c(2000:2010)]),mean(s_daily$TMAX[h_daily$YEAR %in% c(2000:2010)])+change_temp_mean_yn))
# print(paste("temp mean",(mean(s_daily$TMIN)+mean(s_daily$TMAX))/2))
# print(paste(mean(tmax_mean,tmin_mean),mean(tmax_mean,tmin_mean)+change_precip_mean_yn))





#aggregate to monthly
h_monthly <- aggregate(h_daily[,c(1:2,4)],FUN=sum,by=list(h_daily[,2], h_daily[,1]))[,c(2,1,5)]  
names(h_monthly)<-names(h_daily[,c(1:2,4)])
h_monthly[,c("TMAX","TMIN")] <- aggregate(h_daily[,c(1:2,5:6)],FUN=mean,by=list(h_daily[,2],h_daily[,1]))[,c(5:6)]
#calculate t avg and t diff
h_monthly$TAVG <- (h_monthly[,"TMAX"]+h_monthly[,"TMIN"])/2
h_monthly$ET<-0 #to preserve order
h_monthly$DIF <- h_monthly[,"TMAX"]-h_monthly[,"TMIN"]
#calculate ET
h_monthly$ET <- Hargreaves(h_monthly$TAVG, h_monthly$DIF, basin_param$lat, GCMDays$Jday[1:744], GCMDays$Days[1:744])
#round
h_monthly[,c(3:7)]<-round(h_monthly[,c(3:7)],2) #round all output
print("h monthly")


#aggregate to monthly
s_monthly <- aggregate(s_daily[,c(1:2,4)],FUN=sum,by=list(s_daily[,2], s_daily[,1]))[,c(2,1,5)]  
names(s_monthly)<-names(s_daily[,c(1:2,4)])
s_monthly[,c("TMAX","TMIN")] <- aggregate(s_daily[,c(1:2,5:6)],FUN=mean,by=list(s_daily[,2],s_daily[,1]))[,c(5:6)]
#calculate t avg and t diff
s_monthly$TAVG <- (s_monthly[,"TMAX"]+s_monthly[,"TMIN"])/2
s_monthly$ET<-0 #to preserve order
s_monthly$DIF <- s_monthly[,"TMAX"]-s_monthly[,"TMIN"]
#calculate ET
s_monthly$ET <- Hargreaves(s_monthly$TAVG, s_monthly$DIF, basin_param$lat, GCMDays$Jday[1:744], GCMDays$Days[1:744])
#round
s_monthly[,c(3:7)]<-round(s_monthly[,c(3:7)],2) #round all output
print("s monthly")


plot(as.numeric(row.names(h_monthly))[1:10],h_monthly$PRCP[1:10],col="grey",pch=16)
points(as.numeric(row.names(s_monthly))[1:10],h_monthly$PRCP[1:10],col="steelblue",pch=16)

plot(row.names(h_daily)[1000:1365],h_daily$PRCP[1000:1365],col="grey",pch=16)
points(row.names(s_daily)[1000:1365],s_daily$PRCP[1000:1365],col="steelblue",pch=16)
plot(row.names(h_daily)[1:365],h_daily$TMIN[1:365],col="grey",pch=16)
points(row.names(s_daily)[1:365],s_daily$TMIN[1:365],col="steelblue",pch=16)

plot(h_monthly$YEAR,h_monthly$TMIN,col="grey",pch=16)
points(s_monthly$YEAR,h_monthly$TMIN,col="steelblue",pch=16)

h_monthly[1:15,]
s_monthly[1,]
h_daily[1:15,]
s_daily[1:15,]

write.table(h_monthly,"hist_monthly_weather.txt",row.names=FALSE,quote=FALSE,col.names=TRUE) # this is the mean monthly timeseries
write.csv(h_monthly,"hist_monthly_weather.csv",row.names=FALSE,quote=FALSE)


