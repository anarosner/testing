#conversion function
c.to.f<-function(temp) {
   temp*(9/5) + 32
}
mm.to.in<-function(mm) {
   mm*0.0393701
}

#column info
met.cols<-c("year","month","day","precip","tmax","tmin","wind")
p.cols<-c(1:4)
t.cols<-c(1:3,5:7)

#load historic data
setwd("C:/ALR/GitHub/somepath/basins/6780817")
ph<-read.table("met_data.txt",header=FALSE)
th<-ph
# colnames(ph) <- met.cols
# th<-ph[,t.cols]
# ph<-ph[,p.cols]


#load scott's simulation output

setwd("C:/ALR/GitHub/testing/temp_files/from Scott/Simulations")
p0.8<-read.table("PRCP_MEAN_CHANGES_80_TEMP_CHANGES_0.txt")
p0.9<-read.table("PRCP_MEAN_CHANGES_90_TEMP_CHANGES_1.txt")
p1<-read.table("PRCP_MEAN_CHANGES_100_TEMP_CHANGES_2.txt")
p1.1<-read.table("PRCP_MEAN_CHANGES_110_TEMP_CHANGES_4.txt")
p1.2<-read.table("PRCP_MEAN_CHANGES_120_TEMP_CHANGES_5.txt")

setwd("C:/ALR/GitHub/somepath/basins/west_brook")
#process precip
for ( i in c(seq(from=0.9,to=1.2,by=0.1),"h") ) {
   print(i)
   s.precip<-get(paste0("p",i))
   #get only precip columns and label
   colnames(s.precip) <- met.cols
   s.precip<-s.precip[,p.cols]
   #create col in inches, water year
   s.precip$precip.in<-s.precip$precip*0.0393701   #in inches
   s.precip$water.year<-s.precip$year
   s.precip[s.precip$month>=10,"water.year"]<-s.precip[s.precip$month>=10,"water.year"]+1
   ##ADD something here to deal w/ partial year created by splitting into water year (data is complete calendar years)
   #aggregate by water year
   annual<-aggregate(s.precip,by=list(s.precip$water.year),FUN=sum)
   annual<-annual[,c(1,5)]
   colnames(annual)<-c("water.year","precip")
   annual$precip.in<-annual$precip*0.0393701   #in inches
   row.names(annual)<-annual$water.year
   s.precip$annual<-annual[as.character(s.precip$water.year),"precip"]
   s.precip[,c("annual","annual.in")]<-annual[as.character(s.precip$water.year),c("precip","precip.in")]
   write.csv( s.precip, file=paste0("precip",i,".csv"),quote=F,row.names=F)
   write.csv( annual, file=paste0("precipannual",i,".csv"),quote=F,row.names=F)
   s.precip[10001:10010,]
   annual[101:110,]   
#    if (!exists("all.precip")) {
#       all.precip<-s.precip[,c(1:6)]
#       all.precipannual<-annual[,1]
#    }
#    all.precip[,ncol(all.precip)+1]<-s.precip[,"precip.in"]
#    names(all.precip)[ncol(all.precip)]<-paste0("p",i)
#    all.precipannual[,paste0("p",i)]<-annual[,"precip.in"]
#    names(all.precipannual)[ncol(all.precipannual)]<-paste0("p",i)
   
}



#read in scott's data: temp
setwd("C:/ALR/GitHub/testing/temp_files/from Scott/Simulations")
t0<-read.table("PRCP_MEAN_CHANGES_80_TEMP_CHANGES_0.txt")
t1<-read.table("PRCP_MEAN_CHANGES_90_TEMP_CHANGES_1.txt")
t2<-read.table("PRCP_MEAN_CHANGES_100_TEMP_CHANGES_2.txt")
t3<-read.table("PRCP_MEAN_CHANGES_100_TEMP_CHANGES_3.txt")
t4<-read.table("PRCP_MEAN_CHANGES_110_TEMP_CHANGES_4.txt")
t5<-read.table("PRCP_MEAN_CHANGES_120_TEMP_CHANGES_5.txt")

#process temp
setwd("C:/ALR/GitHub/somepath/basins/west_brook")
for ( i in c(0:5,"h") ) {
   print(i)
   s.temp<-get(paste0("t",i))
   #get only precip columns and label
   colnames(s.temp) <- met.cols
   s.temp<-s.temp[,t.cols]
   #create col in F
   s.temp[,"tmax.f"]<-c.to.f(s.temp[,"tmax"])   #in F
   s.temp[,"over.80"]<-as.numeric(s.temp[,"tmax.f"]>=80)
   s.temp[,"over.90"]<-as.numeric(s.temp[,"tmax.f"]>=90)
   #aggregate by calendar year
   annual<-aggregate(s.temp,by=list(s.temp$year),FUN=sum)
   annual<-annual[,c(1,9,10)]
   colnames(annual)<-c("year","days.over.80","days.over.90")
   write.csv( s.temp, file=paste0("temp",i,".csv"),quote=F,row.names=F)
   write.csv( annual, file=paste0("tempannual",i,".csv"),quote=F,row.names=F)
}



tempannual<-annual
sort1<-tempannual[order(tempannual$days.over.90),]
n<-nrow(tempannual)
p10<-sort1[round(n*0.1)-1,]
p50<-sort1[round(n*0.5)-1,]
p90<-sort1[round(n*0.9)-1,]
p99<-sort1[round(n*0.99)-1,]
p10<-sort1[round(n*0.1)-1,]
sort1













#load historic data
setwd("C:/ALR/GitHub/somepath/basins/6780817")
h_daily<-read.table("met_data.txt",header=FALSE)
colnames(h_daily) <- c("YEAR","MONTH","DAY","PRCP","TMAX","TMIN","WIND")


# colnames(t0) <- met.cols
# t0<-t0[,t.cols]
# colnames(t1)<-met.cols
# t1<-t1[,t.cols]
# colnames(t2)<-met.cols
# t2<-t2[,t.cols]
# colnames(t3)<-met.cols
# t3<-t3[,t.cols]
# colnames(t4)<-met.cols
# t4<-t4[,t.cols]
# colnames(t5)<-met.cols
# t5<-t5[,t.cols]


#aggregate 30-day precip
for ( i in seq(from=0.8,to=1.2,by=0.1) ) {
   #get only precip columns and label
   s.precip<-get(paste0("p",i))
   colnames(s.precip) <- met.cols
   s.precip<-s.precip[,p.cols]
   #create 
}

# y <- t0[1:1000,"precip"]
sum.30d<-seq(from=0, to=0, length.out=29)
sum.30d<-c( rolling.sum, rollapply( zoo ( t0[,"precip"] ), 30, sum ) )
sum.1yr<-seq(from=0, to=0, length.out=364)
sum.1yr<-c( rolling.sum, rollapply( zoo ( t0[,"precip"] ), 365, sum ) )

length(rolling.sum)



#write to csv
setwd("C:/ALR/GitHub/somepath/basins/west_brook")
for ( i in seq(from=0.8,to=1.2,by=0.1) ) {
   write.csv( get(paste0("p",i)), file=paste0("precip",i,".csv"),quote=F,row.names=F)
}


setwd("C:/ALR/GitHub/somepath/basins/west_brook")
for ( i in 0:5 ) {
   write.csv( get(paste0("t",i)), file=paste0("temp",i,".csv"),quote=F,row.names=F)
   get(paste0("t",i))[1:10,]
   
}






#put all precip change simulations data in single data frame
s.precip<-p1[,c("year","month","day")]
c<-ncol(s.precip)
for ( i in seq(from=0.8,to=1.2,by=0.1) ) {
   c<-c+1
   s.precip[,c]<-get(paste0("p",i))[,"precip"]
   names(s.precip)[c]<-paste0("p",i)
}
s.precip[1:10,]

setwd("C:/ALR/GitHub/somepath/data/climate_data")
write.csv(s.precip,file="precip_simulations.csv",row.names=F,quote=F)
write.csv(s.precip[,c(1:4)],file="precip_simulations08.csv",row.names=F,quote=F)
# write.table(s.precip,file="precip_simulations.txt",row.names=F,quote=F,sep=" ",col.names=T)

#put all temp change simulation data in single data frame
s.temp<-t0[,c("year","month","day")]
c<-ncol(s.temp)
for ( i in 0:5 ) {
   c<-c+1
   s.temp[,c]<-get(paste0("t",i))[,"tmax"]
   names(s.temp)[c]<-paste0("tmax",i)
   c<-c+1
   s.temp[,c]<-get(paste0("t",i))[,"tmin"]
   names(s.temp)[c]<-paste0("tmin",i)
   c<-c+1
   s.temp[,c]<-get(paste0("t",i))[,"wind"]
   names(s.temp)[c]<-paste0("wind",i)
}
s.temp[1:10,]
write.csv(s.temp,file="temp_simulations.csv",row.names=F,quote=F)

ncol(s.temp)
ncol(s.precip)



#load historic data
setwd("C:/ALR/GitHub/somepath/basins/6780817")
h_daily<-read.table("met_data.txt",header=FALSE)
colnames(h_daily) <- c("YEAR","MONTH","DAY","PRCP","TMAX","TMIN","WIND")
h_daily[1:10,]