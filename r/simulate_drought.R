#!/usr/bin/env Rscript
print("starting simulate drought script")

this_dir<-getwd()
source("./supporting_scripts/param_startup.R")
source("./supporting_scripts/plot_functions.R")
source("./supporting_scripts/Hargreaves.R") #This generates ET values and may get moved to the met section

GCMDays <- read.table(paste0(args$data_dir,"/flow_data/Days_For_ABCDE_GCM.txt"),header=TRUE)


#parse user input params
change_precip_mean <- parse.param("precip_mean_y1")/100+1
change_temp_mean <- parse.param("temp_mean_y1")
drought_annual_inches <- parse.param("drought_annual_inches")
# percentile<-parse.param("percentile")/100

#load data
basin_id <- parse.param("basin_id")
basin_param <- fromJSON(file=paste0(basin_dir,"/",basin_id,"/param.json"))


# h.precipannual<-read.csv(paste0(basin_dir,"/",basin_id,"/precipannualh.csv"),header=T)
all.precipannual<-read.csv(paste0(basin_dir,"/",basin_id,"/precipannual",change_precip_mean,".csv"),header=T)
all.precip<-read.csv(paste0(basin_dir,"/",basin_id,"/precip",change_precip_mean,".csv"),header=T)
all.temp<-read.csv(paste0(basin_dir,"/",basin_id,"/temp",change_temp_mean,".csv"),header=T)

start.year<-min(all.precipannual$water.year)
end.year<-max(all.precipannual$water.year)


setwd(file.path(basin_dir,"west_brook"))
all.precipannual<-read.csv("precipannual0.9.csv")
all.precip<-read.csv("precip0.9.csv")
all.temp<-read.csv("temp0.csv")
GCMDays <- read.table("c:/documents/github/somepath/data/flow_data/Days_For_ABCDE_GCM.txt",header=TRUE)

# s.tempannual<-read.csv("tempannual0.csv")
drought_annual_inches<-40

# h.precipannual<-h.precipannual[order(h.precipannual$precip.in),]
all.precipannual<-all.precipannual[rev(order(all.precipannual$precip.in)),]
row.names(all.precipannual)<-NULL
# row.names(all.precipannual)<-all.precipannual$water.year

#find the year with the highest amount of rain below the user-given threshold
row.annual<-min(as.numeric(row.names(all.precipannual[all.precipannual$precip.in<drought_annual_inches,])))
drought.year<-all.precipannual[row.annual,"water.year"]
#define a 10-yr period with the drought year in the middle 
drought.period<-seq(from=drought.year-4,to=drought.year+5)
{ #in the unlikely case that the selected year is close to the start or end of the 1,000 year time series
if (drought.period[1]<start.year)
  drought.period<-seq(from=start.year,length.out=10)
else if (drought.period[1]>end.year)
  drought.period<-seq(to=end.year,length.out=10)
}
drought.series<-  all.precip[all.precip$water.year %in% drought.period,]

#add temp data for same simulation year, from generated data of user-specified temp change
temp.cols<-c("tmax", "tmin", "wind")
drought.series[,temp.cols] <- all.temp[as.numeric(row.names(drought.series)),temp.cols]
drought.series[,"tavg"]<-apply(drought.series[,c("tmax","tmin")],1,mean) 
      #not really necessary since monthly and annual mean taken below, 
      #   but aggregate at the daily time step, in case we want to look later @ something like monthly max tavg 
drought.series[,"tdif"]<-drought.series[,"tmax"]-drought.series[,"tmin"]

# drought.series[,c(9:14)] <- all.temp[as.numeric(row.names(drought.series)),c(1:6)]  #testing
      # dim(drought.series)
      # drought.series[1:10,]
      # drought.series[1800:1810,]
      # all.precip[all.precip$month==2 & all.precip$day==29,]  they're all 365-day years


###change here down
#aggregate to monthly
#total precip
precip.monthly <- aggregate(drought.series, FUN=sum,
                            by=list(drought.series[,"month"], 
                                    drought.series[,"year"]))[,c("Group.2","Group.1","precip.in")]  
#average temp
temp.monthly<-aggregate(drought.series,FUN=mean,
                        by=list(drought.series[,"month"],
                                drought.series[,"year"]))[,c("Group.2","Group.1","tavg","tdif")]
climate.monthly<-cbind(precip.monthly,temp.monthly)
names(climate.monthly)[1]<-"year"
names(climate.monthly)[2]<-"month"
climate.monthly$water.year<-climate.monthly$year
climate.monthly[climate.monthly$month>=10,"water.year"]<-climate.monthly[climate.monthly$month>=10,"water.year"]+1
climate.monthly<-climate.monthly[,c(8,1:3,6:7)]

#change year to 1-10 (so as not to be confused with a forecast for a particular year)
climate.monthly$year<-climate.monthly$year-climate.monthly$year[1]+1
climate.monthly$water.year<-climate.monthly$water.year-climate.monthly$water.year[1]+1

climate.monthly[1:10,]


climate.monthly$ET <- Hargreaves(climate.monthly$tavg, climate.monthly$tdif, basin_param$lat, GCMDays$Jday[1:744], GCMDays$Days[1:744])


setwd(run_dir)
write.table(h_monthly,"hist_monthly_weather.txt",row.names=FALSE,quote=FALSE,col.names=TRUE) # this is the mean monthly timeseries
write.csv(h_monthly,"hist_monthly_weather.csv",row.names=FALSE,quote=FALSE)

write.table(h_daily,"daily_weather.txt",row.names=FALSE,quote=FALSE) #This is the daily timeseries
write.csv(h_daily,"daily_weather.csv",row.names=FALSE,quote=FALSE)

h.clim<-h_monthly
plot.thumbnail(type="historic") #chagne to "single.precip"
# plot.thumbnail(type="airtemp")





# # row.annual
# # 
# # all.precipannual[580:590,]
# 
# # sort1<-h.precipannual[order(h.precipannual$precip.in),]
# # sort2<-all.precipannual[order(all.precipannual$precip.in),]
# # hist(sort2[,"precip.in"],breaks=seq(from=floor(min(sort2$precip.in)/10)*10, to=ceiling(max(sort2$precip.in)/10)*10, by=2),freq=F,border="red",add=T)
# # hist(sort1[-1,"precip.in"],breaks=seq(from=floor(min(sort1$precip.in[-1])/10)*10, to=ceiling(max(sort1$precip.in)/10)*10, by=2),freq=F,add=F,border="blue")
# # quantile(h.precipannual$precip.in,c(.01,.1,.25,.5,.75,.9,.99))
# # quantile(all.precipannual$precip.in,c(.01,.1,.25,.5,.75,.9,.99))
# 
# 
# # plot(all.precipannual$water.year,all.precipannual$precip.in,xlim=c(min(h.precipannual$water.year),max(all.precipannual$water.year)))
# # abline(h=median(all.precipannual$precip.in))
# # points(h.precipannual$water.year,h.precipannual$precip.in,col="red")
# # abline(h=median(h.precipannual$precip.in),col="red")
# 
# 
# 
# # value<-sort[round(nrow(sort)*percentile)-1,"precip.in"]
# # value
# 
# i<-ceiling(runif(1,min=0,max=nrow(precipannual)))
# flag<-0
# while (flag==0) {
# #    print(i)
# #    print(precipannual$precip.in[i])
#    {
#    if ( precipannual$precip.in>=floor(value-1) & precipannual$precip.in<=ceiling(value+1) )
#       flag<-1
#    else { 
#       if (i+1>nrow(precipannual) )
#          i<-1
#       else
#          i<-i+1
#       }
#    }
# }
# print(i)
# print(precipannual$precip.in[i])
# 
# 
# precip<-read.csv(paste0(basin_dir,"/",basin_id,"/precipannual.csv"),header=T)
# 
