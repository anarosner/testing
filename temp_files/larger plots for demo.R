windowsFonts(
 f3=windowsFont("Gill Sans MT")
)

#mean annual precip

#define variables & parameters used for this plot 
p.h<-c(seq(from=20,to=max(c(h.precip$PRCP,s.precip$PRCP)),by=10),sprintf("%.1f",mean(h.precip$PRCP)))
p.y<-c(min(c(h.precip$PRCP,s.precip$PRCP))-3,max(c(h.precip$PRCP,s.precip$PRCP)))
p.x<-c(min(h.precip$y),max(s.precip$y))
p.s<-0.7

svg(filename="precip.svg",width=8,height=6)


    par(mai=c(.5,.5,.4,.3)) #svg (inches)
#          par(mai=c(.75,.75,.1,.1)) #screen
    par(fg="white",col.main="steelblue4",cex=p.s,cex.main=1.8)
    par(family="serif")
    par(mgp=c(1,.1,0)) #margin b/w plot and axis labels
    par(cex.lab=1.2)

    plot(h.precip$y,h.precip$PRCP,pch=18,col="dodgerblue2",xlim=p.x, ylim=p.y,axes=F,xlab="",ylab="precipitation (inches)",col.lab="slategray4")
    title(main="Mean Annual Precipitation", family="f3")
    axis(side=2,at=p.h,col="white", las=2,cex.axis=0.9,col.axis="slategray4") 
    axis(side=1,col="white", las=1,cex.axis=1.1,col.axis="slategray4") 
    points(s.precip$y,s.precip$PRCP,pch=19,col="dodgerblue4",cex=p.s)
    abline(h=p.h,col="grey",lty=3)
    abline(h=mean(h.precip$PRCP),col="dodgerblue2",lty=2)

dev.off()



#monthly avg temp

#define variables & parameters used for this plot 
p.h1<-rev(seq(from=0,to=min(c(h.jantemp$TAVG,s.jantemp$TAVG)),by=-10))
p.h2<-seq(from=0,to=max(c(s.julytemp$TAVG,h.julytemp$TAVG)),by=10)
p.h<-c(p.h1,p.h2[-1])
p.y<-c(min(c(h.jantemp$TAVG,s.jantemp$TAVG)),max(c(s.julytemp$TAVG,h.julytemp$TAVG)))
p.x<-c(min(h.jantemp$YEAR),max(s.jantemp$YEAR))
p.s<-0.7
p.sl<-1.1

#write svg
svg(filename="temp.svg",width=8,height=6)
  par(mai=c(.5,.75,.4,.3)) #svg (inches)
  #          par(mai=c(.75,.75,.1,.1)) #screen
  par(fg="white",col.main="steelblue4",cex=p.s,cex.main=1.8)
  par(family="serif")
  par(mgp=c(.8,.1,0))
  par(cex.lab=1.2)
  text.main<-paste0("January and July Daily Average Air Temperatures")
  text.axis<-paste0("temperatures (",iconv("\xb0","latin1","UTF-8"),"C)")

  plot(h.jantemp$YEAR,h.jantemp$TAVG,pch=18,col="dodgerblue2",xlim=p.x, ylim=p.y,axes=F,xlab="",ylab=text.axis,col.lab="slategray4")
  title(main=text.main,col="black",family="f3")
  axis(side=2,at=p.h,col="white", las=2,cex.axis=p.sl,col.axis="slategray4") 
  axis(side=1,col="white", las=1,cex.axis=p.sl,col.axis="slategray4") 
  points(s.jantemp$YEAR,s.jantemp$TAVG,pch=19,col="dodgerblue4",cex=p.s)
  points(h.julytemp$YEAR,h.julytemp$TAVG,pch=18,col="orangered2",cex=p.s)
  points(s.julytemp$YEAR,s.julytemp$TAVG,pch=19,col="orangered4",cex=p.s)
  abline(h=p.h,col="grey",lty=3)
dev.off()


#monthly streamflow avg
    p.h<-c(seq(from=0,to=max(c(max(h.max$flow),max(s.max$flow))),by=100))
    p.s<-0.7
    p.sl<-1.1
    ### annual min, max, and average      
    svg(filename="flow.svg",width=8,height=6)
        par(mai=c(.5,.75,.4,.3)) #svg (inches)
        #          par(mai=c(.75,.75,.1,.1)) #screen
        par(fg="white",col.main="steelblue4",cex=p.s,cex.main=1.8)
        par(family="serif")
        par(mgp=c(1.1,.1,0))
        par(cex.lab=1.2)
        #           plot(h.annual$year,h.annual$flow,xlim=c(min(h.annual$year),max(s.annual$year)),ylim=c(0,max(h.max$flow)),lwd=2,col="gray40",axes=F,xlab="",ylab="",pch=16,cex=p.s)
        #           points(s.annual$year,s.annual$flow,pch=18,lty=5,lwd=2,col="gray40",cex=p.s)
        
        plot(h.max$year,h.max$flow,xlim=c(min(h.annual$year),max(s.annual$year)),ylim=c(0,max(h.max$flow)),lwd=2,col="steelblue",axes=F,xlab="",ylab="streamflow",pch=16,cex=p.s,col.lab="slategray4")
        #         points(h.max$year,h.max$flow,lty=1,col="steelblue",pch=16,cex=p.s)
        points(s.max$year,s.max$flow,lty=5,col="steelblue",pch=18,cex=p.s)
        title(main="Annual Streamflow: Minimum and Maximum",col="black", family="f3")
        points(h.min$year,h.min$flow,lty=1,col="salmon",pch=16,cex=p.s)
        points(s.min$year,s.min$flow,lty=5,col="salmon",pch=18,cex=p.s)
        # legend(x="topright",legend(""))
        
        axis(side=2,at=p.h,col="white", las=2,cex.axis=p.sl,col.axis="slategray4") 
        axis(side=1,col="white", las=1,cex.axis=p.sl,col.axis="slategray4") 
        abline(h=p.h,col="grey",lty=3)
    
    
    dev.off()





#monthly streamtemp
# p.h<-c(seq(from=0,to=max(c(max(h.max$flow),max(s.max$flow))),by=100))
# p.s<-0.7
# p.sl<-1.1
# ### annual min, max, and average      
# svg(filename="flow.svg",width=8,height=6)
# par(mai=c(.5,.75,.4,.3)) #svg (inches)
# #          par(mai=c(.75,.75,.1,.1)) #screen
# par(fg="white",col.main="steelblue4",cex=p.s,cex.main=1.8)
# par(family="serif")
# par(mgp=c(1.1,.1,0))
# par(cex.lab=1.2)
# #           plot(h.annual$year,h.annual$flow,xlim=c(min(h.annual$year),max(s.annual$year)),ylim=c(0,max(h.max$flow)),lwd=2,col="gray40",axes=F,xlab="",ylab="",pch=16,cex=p.s)
# #           points(s.annual$year,s.annual$flow,pch=18,lty=5,lwd=2,col="gray40",cex=p.s)
# 
# plot(h.max$year,h.max$flow,xlim=c(min(h.annual$year),max(s.annual$year)),ylim=c(0,max(h.max$flow)),lwd=2,col="steelblue",axes=F,xlab="",ylab="streamflow",pch=16,cex=p.s,col.lab="slategray4")
# #         points(h.max$year,h.max$flow,lty=1,col="steelblue",pch=16,cex=p.s)
# points(s.max$year,s.max$flow,lty=5,col="steelblue",pch=18,cex=p.s)
# title(main="Annual Streamflow: Minimum and Maximum",col="black", family="f3")
# points(h.min$year,h.min$flow,lty=1,col="salmon",pch=16,cex=p.s)
# points(s.min$year,s.min$flow,lty=5,col="salmon",pch=18,cex=p.s)
# # legend(x="topright",legend(""))
# 
# axis(side=2,at=p.h,col="white", las=2,cex.axis=p.sl,col.axis="slategray4") 
# axis(side=1,col="white", las=1,cex.axis=p.sl,col.axis="slategray4") 
# abline(h=p.h,col="grey",lty=3)
# 
# 
# dev.off()