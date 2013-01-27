#!/usr/bin/env Rscript
print("starting historic baseline climate script")

this_dir<-getwd()
source("./supporting_scripts/param_startup.R")
source("./supporting_scripts/plot_functions.R")
source("Hargreaves.R") #This generates ET values and may get moved to the met section


#parse user input params
precip_mean_y1 <- parse.param("precip_mean_y1")/100 + 1
precip_mean_yn <- parse.param("precip_mean_yn")/100 + 1
precip_var_y1 <- parse.param("precip_var_y1")/100 + 1
precip_var_yn <- parse.param("precip_var_yn")/100 + 1
temp_mean_y1 <- parse.param("temp_mean_y1")
temp_mean_yn <- parse.param("temp_mean_yn")


#load data
met_data<-read.table(paste0(basin_dir,"/",basin_id,"/met_data.txt"),header=FALSE)
colnames(met_data) <- c("YEAR","MONTH","DAY","PRCP","TMAX","TMIN","WIND")

n_years<-met_data[nrow(met_data),"YEAR"]-met_data[1,"YEAR"]+1
y1<-met_data[1,"YEAR"]
yn<-met_data[nrow(met_data),"YEAR"]
n_years<-yn-y1

GCMDays <- read.table(paste0(args$data_dir,"/flow_data/Days_For_ABCDE_GCM.txt"),header=TRUE)

#baseline shift
h_daily<-met_data


mean_precip<-mean(h_daily$PRCP)
std_precip<-sd(h_daily$PRCP)
mean_tmin<-mean(h_daily$TMIN)
mean_tmax<-mean(h_daily$TMAX)

mean_precip_inc<-nrow(h_daily)


# days_in_year<-aggregate(GCMDays,FUN=sum,by=list(year=GCMDays$Year))[,c("year","Days")]
# names(days_in_year[,2])<-"days"





#aggregate to monthly
hist_monthly <- aggregate(met_data[,c(1:2,4)],FUN=sum,by=list(met_data[,2], met_data[,1]))[,c(1,2,5)]  #FINAL_HISTORIC_MONTHLY2
names(hist_monthly)<-names(met_data[,c(1:2,4)])
hist_monthly[,c("TMAX","TMIN")] <- aggregate(met_data[,c(1:2,5:6)],FUN=mean,by=list(met_data[,2],met_data[,1]))[,c(5:6)]

# print(hist_monthly[1:4,]

#calculate t avg and t diff
hist_monthly$TAVG <- (hist_monthly[,"TMAX"]+hist_monthly[,"TMIN"])/2
hist_monthly$ET<-0 #to preserve order
hist_monthly$DIF <- hist_monthly[,"TMAX"]-hist_monthly[,"TMIN"]

#calculate ET
hist_monthly$ET <- Hargreaves(hist_monthly$TAVG, hist_monthly$TDIF, BasinLat, GCMDays$Jday[1:744], GCMDays$Days[1:744])

hist_monthly[,c(3:7)]<-round(hist_monthly[,c(3:7)],2) #round all output

write.table(hist_monthly,"h_monthly_weather.txt",row.names=FALSE,quote=FALSE,col.names=TRUE) # this is the mean monthly timeseries
write.csv(hist_monthly,"h_monthly_weather.txt",row.names=FALSE,quote=FALSE)


