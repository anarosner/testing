convert.mm.in<-function(x) {x*0.0393701}

#monthly
h.clim<-FINAL_HISTORIC_MONTHLY
h.clim$PRCP<-convert.mm.in(h.clim$PRCP)
s.clim<-FINAL_STOCHASTIC_MONTHLY
s.clim$PRCP<-convert.mm.in(s.clim$PRCP)

#aggregated annually (calendar year, not water year)
h.precip<-aggregate(h.clim,by=list(y=h.clim$YEAR),FUN=sum)[,c("y","PRCP")]
s.precip<-aggregate(s.clim,by=list(y=s.clim$YEAR),FUN=sum)[,c("y","PRCP")]

#annual
par(mfrow=c(1,1))
plot(h.precip$y,h.precip$PRCP,xlim=c(1950,2080),ylim=c(20,70))
points(s.precip$y,s.precip$PRCP,pch=16)
abline(h=mean(h.precip$PRCP))
abline(h=mean(s.precip$PRCP),lty=3)
mean(h.precip$PRCP)
mean(s.precip$PRCP)
var(h.precip$PRCP)
var(s.precip$PRCP)
# for this particular run output:
# > mean(h.precip$PRCP)
# [1] 46.99427
# > mean(s.precip$PRCP)
# [1] 45.85178
# > var(h.precip$PRCP)
# [1] 61.58488
# > var(s.precip$PRCP)
# [1] 48.84415


#monthly
plot(h.clim$YEAR,h.clim$PRCP,xlim=c(1950,2080),log="y")
points(s.clim$YEAR,s.clim$PRCP,pch=16,log="y")
mean(h.clim$PRCP)
mean(s.clim$PRCP)
var(h.clim$PRCP)
var(s.clim$PRCP)
# for this run 
# > mean(h.clim$PRCP)
# [1] 3.916189
# > mean(s.clim$PRCP)
# [1] 3.820982
# > var(h.clim$PRCP)
# [1] 4.046618
# > var(s.clim$PRCP)
# [1] 3.904782

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

setwd("C:/ALR/GitHub/testing/temp_files")
svg(filename="dec_jan_feb_avg_temp.svg",width=7,height=7)
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
dev.off()

h.wintemp<-h.clim[h.clim$MONTH %in% c(12,1,2),]
h.wintemp[h.wintemp$MONTH==12,"YEAR"]<-h.wintemp[h.wintemp$MONTH==12,"YEAR"]+1
h.wintemp[1:10,]
s.wintemp<-s.clim[h.clim$MONTH %in% c(12,1,2),]
s.wintemp[s.wintemp$MONTH==12,"YEAR"]<-s.wintemp[s.wintemp$MONTH==12,"YEAR"]+1
s.wintemp[1:10,]


par(col="black",cex=1,cex.main=1)
svg(filename="winter_avg_min_max_temp.svg",width=7,height=7,onefile=F)
   par(mfrow=c(3,1))
   plot(h.wintemp$YEAR,h.wintemp$TAVG,pch=19,xlim=c(min(h.clim$YEAR),max(s.clim$YEAR)),main="Dec,Jan,Feb Avg Temp")
   points(s.wintemp$YEAR,s.wintemp$TAVG,pch=19)
   # legend(x="topright",legend(""))
   plot(h.wintemp$YEAR,h.wintemp$TMIN,pch=19,xlim=c(min(h.clim$YEAR),max(s.clim$YEAR)),main="Dec,Jan,Feb Min Temp")
   points(s.wintemp$YEAR,s.wintemp$TMIN,pch=19)
   
   plot(h.wintemp$YEAR,h.wintemp$TMAX,pch=19,xlim=c(min(h.clim$YEAR),max(s.clim$YEAR)),main="Dec,Jan,Feb Max Temp")
   points(s.wintemp$YEAR,s.wintemp$TMAX,pch=19)
dev.off()

mean(h.wintemp$TAVG)
mean(s.wintemp$TAVG)
var(h.wintemp$TAVG)
var(s.wintemp$TAVG)
#for this output
# > mean(h.wintemp$TAVG)
# [1] -4.337419
# > mean(s.wintemp$TAVG)
# [1] -4.148354
# > var(h.wintemp$TAVG)
# [1] 7.031338
# > var(s.wintemp$TAVG)
# [1] 1.898777
