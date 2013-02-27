rm(list=ls())

setwd("C:/ALR/GitHub/testing/temp_files")
#monthly tmax, tmin, and total precip
#1950-2099
load("met.Rdata")

# load("temp.Rdata")
# load("flow.Rdata")
GCMDays <- read.table("c:/alr/github/somepath/data/flow_data/Days_For_ABCDE_GCM.txt",header=TRUE)


    # dim(final_met)
    # dim(final_flow)
    # dim(final_temp)
    # dim(GCMDays)
    # GCMDays[nrow(GCMDays),]
    # GCMDays[1,]

      #notes from conversation w/ austin
      # 1-1-1950
      # 12-31-2099
      # max monthly
      # min monthly
      # tot monthly precip
      # 
      # 2020-50
      # first 37: B1
      # next  36: A2
      # next 39:  A1b
      # sres<-c(rep("B1",37),rep("A2",36),rep("A1b",39))


final_met[1:10,1:5,1]
#these are monthly
tmax<-as.data.frame(final_met[,,1])
tmin<-as.data.frame(final_met[,,2])
#   mean(c(tmax,tmin))#  ...not sure how to cal
precip<-as.data.frame(final_met[,,3])
# dim(tmax)

sres<-c(rep("B1",37),rep("A2",36),rep("A1b",39))
# length(sres)
# ncol(tmax)
colnames(tmax)<-sres
colnames(tmin)<-sres
colnames(precip)<-sres


#there is one extra year in Austin's gcm data
tmax$year<-c(GCMDays$Year,rep(2099,12))
tmax$month<-c(GCMDays$Month,1:12)
tmin$year<-c(GCMDays$Year,rep(2099,12))
tmin$month<-c(GCMDays$Month,1:12)
precip$year<-c(GCMDays$Year,rep(2099,12))
precip$month<-c(GCMDays$Month,1:12)
ncolumns<-ncol(tmax)
# ncol(tmax)
# ncol(tmin)
# ncol(precip)
tmax<-tmax[,c(ncolumns-1,ncolumns,1:(ncolumns-2))]
tmin<-tmin[,c(ncolumns-1,ncolumns,1:(ncolumns-2))]
precip<-precip[,c(ncolumns-1,ncolumns,1:(ncolumns-2))]
# names(tmax)

tavg<-tmax[,c("year","month")]
#don't judge me: I didn't have time to do it properly, so I'm looping
for(i in 1:nrow(tmax)){
  for(j in 3:ncolumns){
    tavg[i,j]<-mean(c(tmin[i,j],tmax[i,j]))
  }
}
names(tavg)<-names(tmax)


# cbind(tmin[1:10,1:2],tmax[1:10,1:2],tavg[1:10,1:2])
# cbind(tmin[1:10,3:4],tmax[1:10,3:4],tavg[1:10,3:4])
# bkup<-tavg



# tmax[1:10,]
# tmax<-tmax[,c(113,114,1:112)]
# tmin<-tmin[,c(113,114,1:112)]
# precip<-precip[,c(113,114,1:112)]
# colnames(tmax[,c("B1","A2","A1b")])

end<-nrow(tmax)

tmin.start<-colMeans(tmin[1:240,c(-1,-2)])
tmin.end<-colMeans(tmin[end-240:end,c(-1,-2)])
tmin.diff<-(tmin.end-tmin.start)
max(tmin.diff)
min(tmin.diff)
median(tmin.diff)

tmax.start<-colMeans(tmax[1:240,c(-1,-2)])
tmax.end<-colMeans(tmax[end-240:end,c(-1,-2)])
tmax.diff<-(tmax.end-tmax.start)
max(tmax.diff)
min(tmax.diff)

tavg.start<-colMeans(tavg[1:240,c(-1,-2)])
tavg.end<-colMeans(tavg[end-240:end,c(-1,-2)])
tavg.diff<-(tavg.end-tavg.start)
max(tavg.diff)
min(tavg.diff)


precip.start<-colMeans(precip[1:240,c(-1,-2)])
precip.end<-colMeans(precip[end-240:end,c(-1,-2)])
precip.diff<-(precip.end-precip.start)/precip.start
max(precip.diff)
min(precip.diff)
# median(precip.diff)




hotter<-which.max(tavg.diff)
wetter<-which.max(precip.diff)
drier<-which.min(precip.diff)

c(tavg.diff[hotter],precip.diff[hotter])
c(tavg.diff[wetter],precip.diff[wetter])
c(tavg.diff[drier],precip.diff[drier])


#aggregate to monthly
hotter.daily<-as.data.frame(cbind(tavg[,c(1,2,hotter)],)
h_monthly <- aggregate(h_daily[,c(1:2,4)],FUN=sum,by=list(h_daily[,2], h_daily[,1]))[,c(2,1,5)]  
names(h_monthly)<-names(h_daily[,c(1:2,4)])
h_monthly[,c("TMAX","TMIN")] <- aggregate(h_daily[,c(1:2,5:6)],FUN=mean,by=list(h_daily[,2],h_daily[,1]))[,c(5:6)]

#calculated additional values needed for subsequent models
#calculate t avg and t diff
h_monthly$TAVG <- (h_monthly[,"TMAX"]+h_monthly[,"TMIN"])/2
h_monthly$ET<-0 #to preserve order
h_monthly$DIF <- h_monthly[,"TMAX"]-h_monthly[,"TMIN"]
h_monthly$ET <- Hargreaves(h_monthly$TAVG, h_monthly$DIF, basin_param$lat, GCMDays$Jday[1:744], GCMDays$Days[1:744])

