rm(list=ls())

setwd("C:/Documents/GitHub/testing/temp_files")
#monthly tmax, tmin, and total precip
#1950-2099
load("met.Rdata")

# load("temp.Rdata")
# load("flow.Rdata")
GCMDays <- read.table("c:/documents/github/somepath/data/flow_data/Days_For_ABCDE_GCM.txt",header=TRUE)


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

tavg<-tmax[,c("year","month")]
#don't judge me: I didn't have time to do it properly, so I'm looping
for(i in 1:nrow(tmax)){
  for(j in 1:ncol(tmax)){
    tavg[i,j]<-mean(c(tmin[i,j],tmax[i,j]))
  }
}


tmin[1:10,1:2]
tmax[1:10,1:2]
tavg[1:10,1:2]



# tmax[1:10,]
# tmax<-tmax[,c(113,114,1:112)]
# tmin<-tmin[,c(113,114,1:112)]
# precip<-precip[,c(113,114,1:112)]
# colnames(tmax[,c("B1","A2","A1b")])

end<-nrow(tmax)
sres<-c("year","month",sres)

B1.cols<-colnames(tmax)[sres=="B1"]
B1.cols

B1.tmax.start<-mean(colMeans(tmax[1:120,B1.cols]))
B1.tmin.start<-mean(colMeans(tmin[1:120,B1.cols]))
B1.tavg.start<-mean(c(B1.tmin.start,B1.tmax.start))
B1.tmin.start
B1.tmax.start
B1.tavg.start
B1.tmax.end<-mean(colMeans(tmax[end-120:end,B1.cols]))
B1.tmin.end<-mean(colMeans(tmin[end-120:end,B1.cols]))
B1.tavg.end<-mean(c(B1.tmin.end,B1.tmax.end))
B1.tmin.end
B1.tmax.end
B1.tavg.end
print("b1 sres scenario")
print(paste("average estimate degrees change from first ten years starting",tmax$year[1],"to the last ten years ending",tmax$year[end]))
(B1.tavg.end-B1.tavg.start)


A2.cols<-colnames(tmax)[sres=="A2"]
A2.cols
A2.tmax.start<-mean(colMeans(tmax[1:120,A2.cols]))
A2.tmin.start<-mean(colMeans(tmin[1:120,A2.cols]))
A2.tavg.start<-mean(c(A2.tmin.start,A2.tmax.start))
A2.tmin.start
A2.tmax.start
A2.tavg.start
A2.tmax.end<-mean(colMeans(tmax[end-120:end,A2.cols]))
A2.tmin.end<-mean(colMeans(tmin[end-120:end,A2.cols]))
A2.tavg.end<-mean(c(A2.tmin.end,A2.tmax.end))
A2.tmin.end
A2.tmax.end
A2.tavg.end
print("A2 sres scenario")
print(paste("average estimate degrees change from first ten years starting",tmax$year[1],"to the last ten years ending",tmax$year[end]))
(A2.tavg.end-A2.tavg.start)


A1b.cols<-colnames(tmax)[sres=="A1b"]
A1b.cols
A1b.tmax.start<-mean(colMeans(tmax[1:120,A1b.cols]))
A1b.tmin.start<-mean(colMeans(tmin[1:120,A1b.cols]))
A1b.tavg.start<-mean(c(A1b.tmin.start,A1b.tmax.start))
A1b.tmin.start
A1b.tmax.start
A1b.tavg.start
A1b.tmax.end<-mean(colMeans(tmax[end-120:end,A1b.cols]))
A1b.tmin.end<-mean(colMeans(tmin[end-120:end,A1b.cols]))
A1b.tavg.end<-mean(c(A1b.tmin.end,A1b.tmax.end))
A1b.tmin.end
A1b.tmax.end
A1b.tavg.end
print("A1b sres scenario")
print(paste("average estimate degrees change from first ten years starting",tmax$year[1],"to the last ten years ending",tmax$year[end]))
(A1b.tavg.end-A1b.tavg.start)


###########################3


precip.annual<-aggregate(precip[,c(3:ncol(precip))],by=list(year=precip$year),FUN=sum)
end<-nrow(precip.annual)
B1.precip.start<-mean(colMeans(precip.annual[1:120,B1.cols]))
B1.precip.end<-mean(colMeans(precip.annual[end-120:end,B1.cols]))
B1.precip.start
B1.precip.end
print("b1 sres scenario")
print(paste("average % change annual precip from first ten years starting",precip$year[1],"to the last ten years ending",precip$year[end]))
(B1.precip.end-B1.precip.start)/(tmax$year[end]-tmax$year[1])


A2.precip.start<-mean(colMeans(precip.annual[1:120,A2.cols]))
A2.precip.end<-mean(colMeans(precip.annual[end-120:end,A2.cols]))
A2.precip.start
A2.precip.end
print("A2 sres scenario")
print(paste("average % change annual precip from first ten years starting",precip$year[1],"to the last ten years ending",precip$year[end]))
(A2.precip.end-A2.precip.start)/(tmax$year[end]-tmax$year[1])


A1b.precip.start<-mean(colMeans(precip.annual[1:120,A1b.cols]))
A1b.precip.end<-mean(colMeans(precip.annual[end-120:end,A1b.cols]))
A1b.precip.start
A1b.precip.end
print("A1b sres scenario")
print(paste("average % change annual precip from first ten years starting",precip$year[1],"to the last ten years ending",precip$year[end]))
(A1b.precip.end-A1b.precip.start)/(tmax$year[end]-tmax$year[1])















gcm.met<-as.data.frame(final_met)
names(gcm.met)
dim(gcm.met)
dim(final_met)
gcm.met[1:10,1,]
gcm.met[1,1:10,1]

gcm.met[1:10,1,]

final_temp[1:10]
final_flow[1:10,1:5]

1-1-1950
12-31-2099
max monthly
min monthly
tot monthly precip

2020-50
first 37: B1
next  36: A2
next 39:  A1b
sres<-c(rep("B1",37),rep("A2",36),rep("A1b",39))

GCMDays[1:10,]
nrow(GCMDays)
GCMDays[nrow(GCMDays),]
