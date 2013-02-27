setwd("C:/ALR/GitHub/testing/r")
this_dir<-getwd()
source("./supporting_scripts/Hargreaves.R") #This generates ET values and may get moved to the met section
GCMDays <- read.table("c:/alr/github/somepath/data/flow_data/Days_For_ABCDE_GCM.txt",header=TRUE)
basin_dir<-"C:/ALR/GitHub/somepath/basins/west_brook"
setwd(basin_dir)


    # hotter
    # A1b.7      A1b.7 
    # 2.07929529 0.07671104 
    # 2          1.1

    # wetter
    # A2.1      A2.1 
    # 1.7970034 0.1377916 
    # 2          1.2

    # drier
    # B1.18       B1.18 
    # 1.02010061 -0.02582424 
    # 1          0.9

precip1.1<-read.csv("precip1.1.csv",header=T)
precip1.2<-read.csv("precip1.2.csv",header=T)
precip0.9<-read.csv("precip0.9.csv",header=T)
temp1<-read.csv("temp1.csv",header=T)
temp2<-read.csv("temp2.csv",header=T)
temp1$tavg<-(temp1[,"tmax"]+temp1[,"tmin"])/2
temp2$tavg<-(temp2[,"tmax"]+temp2[,"tmin"])/2

# YEAR MONTH DAY PRCP TMAX TMIN TAVG
# YEAR MONTH PRCP TMAX TMIN TAVG ET DIF

rows_100_yrs<-c(3710:40209)
hotter<-as.data.frame(cbind(precip1.1[rows_100_yrs,c("year","month","day","precip")],temp2[rows_100_yrs,c("tmax","tmin","tavg")]))
wetter<-as.data.frame(cbind(precip1.2[rows_100_yrs,c("year","month","day","precip")],temp2[rows_100_yrs,c("tmax","tmin","tavg")]))
drier<-as.data.frame(cbind(precip0.9[rows_100_yrs,c("year","month","day","precip")],temp1[rows_100_yrs,c("tmax","tmin","tavg")]))


hotter[1:5,]
hotter[(nrow(hotter)-5):nrow(hotter),]
mos<-GCMDays[3:15,2:5]

    
for ( i in c("hotter","wetter","drier") ) {
    setwd("C:/ALR/GitHub/testing/somepath2/for_k")
    #aggregate to monthly
    s_daily<-get(i)
    s_monthly <- aggregate(s_daily[,c(1:2,4)],FUN=sum,by=list(s_daily[,2], s_daily[,1]))[,c(2,1,5)]  
    names(s_monthly)<-names(s_daily[,c(1:2,4)])
    s_monthly[,c("TMAX","TMIN")] <- aggregate(s_daily[,c(1:2,5:6)],FUN=mean,by=list(s_daily[,2],s_daily[,1]))[,c(5:6)]
    
    #calculated additional values needed for subsequent models
    #calculate t avg and t diff
    s_monthly$TAVG <- (s_monthly[,"TMAX"]+s_monthly[,"TMIN"])/2
    s_monthly$ET<-0 #to preserve order
    s_monthly$DIF <- s_monthly[,"TMAX"]-s_monthly[,"TMIN"]
    s_monthly$ET <- Hargreaves(s_monthly$TAVG, s_monthly$DIF, basin_param$lat, rep(mos$Jday,length.out=nrow(s_monthly)), rep(mos$Days,length.out=nrow(s_monthly)))
    #round
    s_monthly[,c(3:7)]<-round(s_monthly[,c(3:7)],2) #round all output
    # print("h monthly")
    # 
    # 
    # h_monthly[1:15,]
    # h_daily[1:15,]
    
    setwd(file.path(getwd(),i))
    names(s_daily)<-c("YEAR","MONTH","DAY","PRCP","TMAX","TMIN","TAVG")
    names(s_monthly)<-c("YEAR","MONTH","PRCP","TMAX","TMIN","TAVG","ET","DIF")
    write.table(s_monthly,"monthly_weather.txt",row.names=FALSE,quote=FALSE,col.names=TRUE) # this is the mean monthly timeseries
#     write.table(s_monthly,"h_monthly_weather.txt",row.names=FALSE,quote=FALSE,col.names=TRUE)
    write.table(s_daily,"daily_weather.txt",row.names=FALSE,quote=FALSE) #This is the daily timeseries
}
