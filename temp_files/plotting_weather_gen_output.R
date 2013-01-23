convert.mm.in<-function(x) {x*0.0393701}

h.clim<-FINAL_HISTORIC_MONTHLY
h.clim$PRCP<-convert.mm.in(h.clim$PRCP)
s.clim<-FINAL_STOCHASTIC_MONTHLY
s.clim$PRCP<-convert.mm.in(s.clim$PRCP)

# names(h.clim)
# "YEAR"  "MONTH" "PRCP"  "TMAX"  "TMIN"  "TAVG"  "ET"    "DIF"
h.jantemp<-h.clim[h.clim$MONTH==1,c("YEAR","TMAX","TMIN","TAVG")]
s.jantemp<-s.clim[h.clim$MONTH==1,c("YEAR","TMAX","TMIN","TAVG")]
h.julytemp<-h.clim[h.clim$MONTH==7,c("YEAR","TMAX","TMIN","TAVG")]
s.julytemp<-s.clim[h.clim$MONTH==7,c("YEAR","TMAX","TMIN","TAVG")]

h.precip<-aggregate(h.clim,by=list(y=h.clim$YEAR),FUN=sum)[,c("y","PRCP")]
s.precip<-aggregate(s.clim,by=list(y=s.clim$YEAR),FUN=sum)[,c("y","PRCP")]
h.pmean<-aggregate(h.clim,by=list(y=h.clim$YEAR),FUN=mean)[,c("y","PRCP")]
h.pcv<-aggregate(h.clim,by=list(y=h.clim$YEAR),FUN=var)[,c("y","PRCP")]
h.pcv$PRCP<-sqrt(h.pcv$PRCP)/h.pmean$PRCP
s.pmean<-aggregate(s.clim,by=list(y=s.clim$YEAR),FUN=mean)[,c("y","PRCP")]
s.pcv<-aggregate(s.clim,by=list(y=s.clim$YEAR),FUN=var)[,c("y","PRCP")]
s.pcv$PRCP<-sqrt(s.pcv$PRCP)/s.pmean$PRCP

plot(h.pcv$y,h.pcv$PRCP,xlim=c(1950,2080))
points(s.pcv$y,s.pcv$PRCP,pch=16)

h.spring<-aggregate(h.clim[h.clim$MONTH %in% c(3,4,5),],by=list(y=h.clim[h.clim$MONTH %in% c(3,4,5),"YEAR"]),FUN=sum)[,c("y","PRCP")]
s.spring<-aggregate(s.clim[s.clim$MONTH %in% c(3,4,5),],by=list(y=s.clim[s.clim$MONTH %in% c(3,4,5),"YEAR"]),FUN=sum)[,c("y","PRCP")]
h.fall<-aggregate(h.clim[h.clim$MONTH %in% c(9,10,11),],by=list(y=h.clim[h.clim$MONTH %in% c(9,10,11),"YEAR"]),FUN=sum)[,c("y","PRCP")]
s.fall<-aggregate(s.clim[s.clim$MONTH %in% c(9,10,11),],by=list(y=s.clim[s.clim$MONTH %in% c(9,10,11),"YEAR"]),FUN=sum)[,c("y","PRCP")]

par(mar=c(5.1, 4.1, 4.1, 2.1))
plot(h.precip$y,h.precip$PRCP,xlim=c(1950,2080),ylim=c(0,100))
points(s.precip$y,s.precip$PRCP,pch=16)
abline(h=mean(h.precip$PRCP))
points(h.spring$y,h.spring$PRCP,pch=21,col="green")
points(s.spring$y,s.spring$PRCP,pch=16,col="green")
points(h.fall$y,h.fall$PRCP,pch=21,col="orange")
points(s.fall$y,s.fall$PRCP,pch=16,col="orange")


aggregate(dummy,by=list(dummy$season),FUN=mean)


par(opar)
# par()$mar
# [1] 5.1 4.1 4.1 2.1

p.s<-0.7
p.sl<-.9
p.m<-paste0("Average January and July air temperatures (",iconv("\xb0","latin1","UTF-8"),"C)")
p.m<-""
par(mai=c(.25,.25,.01,.01))
par(fg="white",col.main="slategray4",cex=p.s,cex.main=p.s)

p.h1<-rev(seq(from=0,to=min(c(h.jantemp$TAVG,s.jantemp$TAVG)),by=-10))
p.h2<-seq(from=0,to=max(c(s.julytemp$TAVG,h.julytemp$TAVG)),by=10)
p.h<-c(p.h1,p.h2[-1])
p.y<-c(min(c(h.jantemp$TAVG,s.jantemp$TAVG)),max(c(s.julytemp$TAVG,h.julytemp$TAVG)))
p.x<-c(min(h.jantemp$YEAR),max(s.jantemp$YEAR))

setwd("C:/ALR/GitHub/testing/temp_files")
# png(filename="thumbnail3.png",width=3,height=3,units="in",res=100)
svg(filename="thumbnail.svg",width=3,height=3)
   plot(h.jantemp$YEAR,h.jantemp$TAVG,pch=18,col="dodgerblue2",main=p.m,xlim=p.x, ylim=p.y,axes=F,xlab="",ylab="")
   axis(side=2,at=p.h,col="white", las=2,cex.axis=p.sl,col.axis="slategray4",family="serif") 
   axis(side=1,col="white", las=1,cex.axis=p.sl,col.axis="slategray4",family="serif") 
   points(s.jantemp$YEAR,s.jantemp$TAVG,pch=19,col="dodgerblue4",cex=p.s)
   points(h.julytemp$YEAR,h.julytemp$TAVG,pch=18,col="orangered2",cex=p.s)
   points(s.julytemp$YEAR,s.julytemp$TAVG,pch=19,col="orangered4",cex=p.s)
   abline(h=p.h,col="grey",lty=3)
dev.off()

p.h<-c(seq(from=0,to=max(c(h.precip$PRCP,s.precip$PRCP)),by=20),sprintf("%.1f",mean(h.precip$PRCP)))
p.y<-c(0,max(c(h.precip$PRCP,s.precip$PRCP)))
p.x<-c(min(h.precip$y),max(s.precip$y))


setwd("C:/ALR/GitHub/testing/temp_files")
# png(filename="thumbnail3.png",width=3,height=3,units="in",res=100)
svg(filename="thumbnail2.svg",width=3,height=3)
   plot(h.precip$y,h.precip$PRCP,pch=18,col="dodgerblue2",main=p.m,xlim=p.x, ylim=p.y,axes=F,xlab="",ylab="")
   axis(side=2,at=p.h,col="white", las=2,cex.axis=p.sl,col.axis="slategray4",family="serif") 
   axis(side=1,col="white", las=1,cex.axis=p.sl,col.axis="slategray4",family="serif") 
   points(s.precip$y,s.precip$PRCP,pch=19,col="dodgerblue4",cex=p.s)
   abline(h=p.h,col="grey",lty=3)
   abline(h=mean(h.precip$PRCP),col="dodgerblue2",lty=2)

dev.off()

#jan and july
plot(h.jantemp$YEAR,h.jantemp$TAVG,pch=18,col="dodgerblue2",xlim=c(min(h.jantemp$YEAR),max(s.jantemp$YEAR)), ylim=c(min(c(h.jantemp$TAVG,s.jantemp$TAVG)),max(c(s.julytemp$TAVG,h.julytemp$TAVG))),fg="slategray3",col.lab="slategray4",col.axis="slategray4",xlab="Year",ylab="Average January and July air temperatures")
plot(h.jantemp$YEAR,h.jantemp$TAVG,pch=18,col="dodgerblue2",xlim=c(min(h.jantemp$YEAR),max(s.jantemp$YEAR)), ylim=c(min(c(h.jantemp$TAVG,s.jantemp$TAVG)),max(c(s.julytemp$TAVG,h.julytemp$TAVG))),fg="white",col.axis="slategray4",xlab="",ylab="",main=m,col.main="slategray4",las=1,cex=p.s,cex.main=p.s,cex.axis=p.s)
plot(h.jantemp$YEAR,h.jantemp$TAVG,pch=18,col="dodgerblue2",xlim=c(min(h.jantemp$YEAR),max(s.jantemp$YEAR)), ylim=c(min(c(h.jantemp$TAVG,s.jantemp$TAVG)),max(c(s.julytemp$TAVG,h.julytemp$TAVG))),fg="white",axes=F,xlab="",ylab="",main=p.m,col.main="slategray4",las=1,cex=p.s,cex.main=p.s,cex.axis=p.s)




#### checking/plotting other winter months
h.jantemp<-h.clim[h.clim$MONTH==1,c("YEAR","TMAX","TMIN","TAVG")]
s.jantemp<-s.clim[h.clim$MONTH==1,c("YEAR","TMAX","TMIN","TAVG")]
h.dectemp<-h.clim[h.clim$MONTH==12,c("YEAR","TMAX","TMIN","TAVG")]
s.dectemp<-s.clim[h.clim$MONTH==12,c("YEAR","TMAX","TMIN","TAVG")]
s.dectemp$YEAR <- s.dectemp$YEAR-1
h.dectemp$YEAR <- h.dectemp$YEAR-1
h.febtemp<-h.clim[h.clim$MONTH==2,c("YEAR","TMAX","TMIN","TAVG")]
s.febtemp<-s.clim[h.clim$MONTH==2,c("YEAR","TMAX","TMIN","TAVG")]

par(mfrow=c(1,1))

plot(h.jantemp$YEAR,h.jantemp$TAVG,pch=18,col="orange",xlim=c(min(h.jantemp$YEAR),max(s.jantemp$YEAR))) 
points(s.dectemp$YEAR,s.dectemp$TAVG,pch=16,col="yellow")
points(s.jantemp$YEAR,s.jantemp$TAVG,pch=16,col="orange")
points(s.febtemp$YEAR,s.febtemp$TAVG,pch=16,col="red")
points(h.dectemp$YEAR,h.dectemp$TAVG,pch=18,col="yellow")
points(h.febtemp$YEAR,h.febtemp$TAVG,pch=18,col="red")
mean(c(h.dectemp$TAVG,h.jantemp$TAVG,h.febtemp$TAVG))
mean(c(s.dectemp$TAVG,s.jantemp$TAVG,s.febtemp$TAVG))
var(c(h.dectemp$TAVG,h.jantemp$TAVG,h.febtemp$TAVG))
var(c(s.dectemp$TAVG,s.jantemp$TAVG,s.febtemp$TAVG))


h.wintemp<-h.clim[h.clim$MONTH %in% c(12,1,2),]
h.wintemp[h.wintemp$MONTH==12,"YEAR"]<-h.wintemp[h.wintemp$MONTH==12,"YEAR"]+1
h.wintemp[1:10,]
s.wintemp<-s.clim[h.clim$MONTH %in% c(12,1,2),]
s.wintemp[s.wintemp$MONTH==12,"YEAR"]<-s.wintemp[s.wintemp$MONTH==12,"YEAR"]+1
s.wintemp[1:10,]

par(mfrow=c(3,1))
par(col="black",cex=1,cex.main=1)
setwd("C:/ALR/GitHub/testing/temp_files")
svg(filename="winter_avg_min_max_temp.svg",width=7,height=7)
   plot(h.wintemp$YEAR,h.wintemp$TAVG,pch=19,xlim=c(min(h.clim$YEAR),max(s.clim$YEAR)),main="Dec,Jan,Feb Avg Temp")
   points(s.wintemp$YEAR,s.wintemp$TAVG,pch=19)
   # legend(x="topright",legend(""))
   plot(h.wintemp$YEAR,h.wintemp$TMIN,pch=19,xlim=c(min(h.clim$YEAR),max(s.clim$YEAR)),main="Dec,Jan,Feb Min Temp")
   points(s.wintemp$YEAR,s.wintemp$TMIN,pch=19)
   
   plot(h.wintemp$YEAR,h.wintemp$TMAX,pch=19,xlim=c(min(h.clim$YEAR),max(s.clim$YEAR)),main="Dec,Jan,Feb Max Temp")
   points(s.wintemp$YEAR,s.wintemp$TMAX,pch=19)
dev.off()





plot(c(1,2,3),c(1,1,1),pch=16,col=rev(heat.colors(3)))
points(c(1,2,3),c(1.2,1.2,1.2),pch=18,col=rev(heat.colors(3)))






plot(1)

plot(1, las = 1)

plot(1, las = 2)

plot(1, las = 3)