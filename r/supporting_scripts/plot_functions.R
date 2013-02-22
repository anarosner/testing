plot.thumbnail<-function(type) {
   #write svg in run directory
   setwd(run_dir)
   
   #set parameters used for all plots
   p.s<-0.7 #point/line scale
   p.sl<-.7 #scale axis lines
   p.m<-1
#    par(mai=c(.25,.25,.01,.01)) #svg (inches)
   par(mai=c(.75,.75,.1,.1)) #screen
   par(fg="white",col.main="slategray4",cex=p.s,cex.main=p.s)
   par(family="serif")
   par(mgp=c(0,.1,0)) #margin b/w plot and axis labels

   
   if (type=="precip") {

      #collect/aggregate data
#       h.clim<-FINAL_HISTORIC_MONTHLY from weather generator
#       s.clim<-FINAL_STOCHASTIC_MONTHLY
      # names(h.clim)
      # "YEAR"  "MONTH" "PRCP"  "TMAX"  "TMIN"  "TAVG"  "ET"    "DIF"
      h.clim$PRCP<-convert.mm.in(h.clim$PRCP)
      s.clim$PRCP<-convert.mm.in(s.clim$PRCP)
      h.precip<-aggregate(h.clim,by=list(y=h.clim$YEAR),FUN=sum)[,c("y","PRCP")]
      s.precip<-aggregate(s.clim,by=list(y=s.clim$YEAR),FUN=sum)[,c("y","PRCP")]
      
      
      #define variables & parameters used for this plot 
      p.h<-c(seq(from=20,to=max(c(h.precip$PRCP,s.precip$PRCP)),by=10),sprintf("%.1f",mean(h.precip$PRCP)))
      p.y<-c(20,max(c(h.precip$PRCP,s.precip$PRCP)))
      p.x<-c(min(h.precip$y),max(s.precip$y))
      
      
      #write svg
      svg(filename="thumbnail2.svg",width=2.5,height=2.5)
        par(mai=c(.4,.35,.4,.01)) #svg (inches)
        #          par(mai=c(.75,.75,.1,.1)) #screen
        par(fg="white",col.main="slategray4",cex=p.s,cex.main=p.s)
        par(family="serif")
        par(mgp=c(0,.1,0)) #margin b/w plot and axis labels
        plot(h.precip$y,h.precip$PRCP,pch=18,col="dodgerblue2",xlim=p.x, ylim=p.y,axes=F,xlab="",ylab="")
         axis(side=2,at=p.h,col="white", las=2,cex.axis=p.sl,col.axis="slategray4") 
         axis(side=1,col="white", las=1,cex.axis=p.sl,col.axis="slategray4") 
         points(s.precip$y,s.precip$PRCP,pch=19,col="dodgerblue4",cex=p.s)
         abline(h=p.h,col="grey",lty=3)
         abline(h=mean(h.precip$PRCP),col="dodgerblue2",lty=2)
      dev.off()
   } #end precip plot type
   
   
   else if (type=="airtemp") {
     
      
      #collect/aggregate data
#       h.clim<-FINAL_HISTORIC_MONTHLY
#       s.clim<-FINAL_STOCHASTIC_MONTHLY
                                    # names(h.clim)
                                    # "YEAR"  "MONTH" "PRCP"  "TMAX"  "TMIN"  "TAVG"  "ET"    "DIF"
      h.jantemp<-h.clim[h.clim$MONTH==1,c("YEAR","TMAX","TMIN","TAVG")]
      s.jantemp<-s.clim[s.clim$MONTH==1,c("YEAR","TMAX","TMIN","TAVG")]
      h.julytemp<-h.clim[h.clim$MONTH==7,c("YEAR","TMAX","TMIN","TAVG")]
      s.julytemp<-s.clim[s.clim$MONTH==7,c("YEAR","TMAX","TMIN","TAVG")]

      
      #define variables & parameters used for this plot 
      p.h1<-rev(seq(from=0,to=min(c(h.jantemp$TAVG,s.jantemp$TAVG)),by=-10))
      p.h2<-seq(from=0,to=max(c(s.julytemp$TAVG,h.julytemp$TAVG)),by=10)
      p.h<-c(p.h1,p.h2[-1])
      p.y<-c(min(c(h.jantemp$TAVG,s.jantemp$TAVG)),max(c(s.julytemp$TAVG,h.julytemp$TAVG)))
      p.x<-c(min(h.jantemp$YEAR),max(s.jantemp$YEAR))

      
      #write svg
      svg(filename="thumbnail.svg",width=2.5,height=2.5)
          par(mai=c(.4,.35,.4,.01)) #svg (inches)
          #          par(mai=c(.75,.75,.1,.1)) #screen
          par(fg="white",col.main="slategray4",cex=p.s,cex.main=p.s)
          par(family="serif")
          par(mgp=c(0,.1,0)) #margin b/w plot and axis labels
      
         plot(h.jantemp$YEAR,h.jantemp$TAVG,pch=18,col="dodgerblue2",xlim=p.x, ylim=p.y,axes=F,xlab="",ylab="")
         axis(side=2,at=p.h,col="white", las=2,cex.axis=p.sl,col.axis="slategray4") 
         axis(side=1,col="white", las=1,cex.axis=p.sl,col.axis="slategray4") 
         points(s.jantemp$YEAR,s.jantemp$TAVG,pch=19,col="dodgerblue4",cex=p.s)
         points(h.julytemp$YEAR,h.julytemp$TAVG,pch=18,col="orangered2",cex=p.s)
         points(s.julytemp$YEAR,s.julytemp$TAVG,pch=19,col="orangered4",cex=p.s)
         abline(h=p.h,col="grey",lty=3)
      dev.off()
      
   } #end airtemp plot type
   
   
   
   if (type=="historic") {
     
     # "YEAR"  "MONTH" "PRCP"  "TMAX"  "TMIN"  "TAVG"  "ET"    "DIF"
     h.clim$PRCP<-convert.mm.in(h.clim$PRCP)
     h.precip<-aggregate(h.clim,by=list(y=h.clim$YEAR),FUN=sum)[,c("y","PRCP")]
     #define variables & parameters used for this plot 
     p.h<-c(seq(from=0,to=max(h.precip$PRCP),by=20),sprintf("%.1f",mean(h.precip$PRCP)))
     p.y<-c(0,max(h.precip$PRCP))
     p.x<-c(min(h.precip$y),max(h.precip$y))
          
     #write svg
     svg(filename="thumbnail.svg",width=2.5,height=2.5)
       par(mai=c(.4,.35,.4,.01)) #svg (inches)
       #          par(mai=c(.75,.75,.1,.1)) #screen
       par(fg="white",col.main="slategray4",cex=p.s,cex.main=p.s)
       par(family="serif")
       par(mgp=c(0,.1,0)) #margin b/w plot and axis labels
       plot(h.precip$y,h.precip$PRCP,pch=18,col="dodgerblue2",xlim=p.x, ylim=p.y,axes=F,xlab="",ylab="")
       axis(side=2,at=p.h,col="white", las=2,cex.axis=p.sl,col.axis="slategray4") 
       axis(side=1,col="white", las=1,cex.axis=p.sl,col.axis="slategray4") 
       abline(h=p.h,col="grey",lty=3)
       abline(h=mean(h.precip$PRCP),col="dodgerblue2",lty=2)
     dev.off()
   } #end historic plot type
   
   
   
   else if (type=="flow") {
      #Collect/aggregate data
                                    #     names(s.flow)
                                    #     [1] "year"  "month" "flow"  
      h.annual<-aggregate(h.flow,list(h.flow$year),FUN=mean)
      s.annual<-aggregate(s.flow,list(s.flow$year),FUN=mean)
      h.min<-aggregate(h.flow,list(h.flow$year),FUN=min)
      s.min<-aggregate(s.flow,list(s.flow$year),FUN=min)
      h.max<-aggregate(h.flow,list(h.flow$year),FUN=max)
      s.max<-aggregate(s.flow,list(s.flow$year),FUN=max)
      
#       h.spring<-aggregate(h.flow[h.flow$month %in% c(3,4,5),],by=list(h.flow[h.flow$month %in% c(3,4,5),"year"]),FUN=mean)
#       s.spring<-aggregate(s.flow[s.flow$month %in% c(3,4,5),],by=list(y=s.flow[s.flow$month %in% c(3,4,5),"year"]),FUN=mean)
#       h.fall<-aggregate(h.flow[h.flow$month %in% c(9,10,11),],by=list(y=h.flow[h.flow$month %in% c(9,10,11),"year"]),FUN=mean)
#       s.fall<-aggregate(s.flow[s.flow$month %in% c(9,10,11),],by=list(y=s.flow[s.flow$month %in% c(9,10,11),"year"]),FUN=mean)
#       h.summer<-aggregate(h.flow[h.flow$month %in% c(6,7,8),],by=list(y=h.flow[h.flow$month %in% c(9,10,11),"year"]),FUN=mean)
#       s.summer<-aggregate(s.flow[s.flow$month %in% c(6,7,8),],by=list(y=s.flow[s.flow$month %in% c(9,10,11),"year"]),FUN=mean)

      #define variables & parameters used for this plot 
      p.h<-c(seq(from=0,to=max(c(max(h.max$flow),max(s.max$flow))),by=100))
#       par(cex=1)
      
      #Write svg plot

      
      
      
      ### annual min, max, and average      
      svg(filename="thumbnail.svg",width=2.5,height=2.5)
         par(mai=c(.4,.35,.4,.01)) #svg (inches)
#          par(mai=c(.75,.75,.1,.1)) #screen
         par(fg="white",col.main="slategray4",cex=p.s,cex.main=p.s)
         par(family="serif")
         par(mgp=c(0,.1,0)) #margin b/w plot and axis labels
      
      #           plot(h.annual$year,h.annual$flow,xlim=c(min(h.annual$year),max(s.annual$year)),ylim=c(0,max(h.max$flow)),lwd=2,col="gray40",axes=F,xlab="",ylab="",pch=16,cex=p.s)
#           points(s.annual$year,s.annual$flow,pch=18,lty=5,lwd=2,col="gray40",cex=p.s)
            
          plot(h.max$year,h.max$flow,xlim=c(min(h.annual$year),max(s.annual$year)),ylim=c(0,max(h.max$flow)),lwd=2,col="steelblue",axes=F,xlab="",ylab="",pch=16,cex=p.s)
#         points(h.max$year,h.max$flow,lty=1,col="steelblue",pch=16,cex=p.s)
          points(s.max$year,s.max$flow,lty=5,col="steelblue",pch=18,cex=p.s)
          
          points(h.min$year,h.min$flow,lty=1,col="salmon",pch=16,cex=p.s)
          points(s.min$year,s.min$flow,lty=5,col="salmon",pch=18,cex=p.s)

          axis(side=2,at=p.h,col="white", las=2,cex.axis=p.sl,col.axis="slategray4") 
          axis(side=1,col="white", las=1,cex.axis=p.sl,col.axis="slategray4") 
          abline(h=p.h,col="grey",lty=3)
          
      
      dev.off()
      
      
      
      
      
      ### monthly
#        plot(s.flow$year,s.flow$flow,log="y",xlim=c(min(h.flow$year),max(s.flow$year)),type="p",col="steelblue")
#        points(h.flow$year,h.flow$flow,pch=19,log="y",col="steelblue")
      
#       
#       lines(h.spring$year,h.spring$flow,lty=1,col="green")
#       lines(s.spring$year,s.spring$flow,lty=3,col="green")
#             
# #       lines(h.fall$year,h.fall$flow,lty=1,col="orange")
# #       lines(s.fall$year,s.fall$flow,lty=3,col="orange")
# 
#       lines(h.summer$year,h.summer$flow,lty=1,col="rosybrown2")
#       lines(s.summer$year,s.summer$flow,lty=3,col="rosybrown2")
      
# 
# 
#       else if (type=="airtemp") {
#         
#         p.h<-seq(from=17,to=20,by=.25)
#         
#         #write svg
#         svg(filename="thumbnail.svg",width=2.5,height=2.5)
#         par(mai=c(.65,.35,.5,.01)) #svg (inches)
#         #          par(mai=c(.75,.75,.1,.1)) #screen
#         par(fg="white",col.main="slategray4",cex=p.s,cex.main=p.s)
#         par(family="serif")
#         par(mgp=c(0,.1,0)) #margin b/w plot and axis labels
#         
#         plot(s.winter$YEAR,s.winter$WB,pch=18,col="dodgerblue2",ylim=c(17.75,18.75),axes=F,xlab="",ylab="",type="l")
#         axis(side=2,at=p.h,col="white", las=2,cex.axis=p.sl,col.axis="slategray4") 
#         axis(side=1,col="white", las=1,cex.axis=p.sl,col.axis="slategray4") 
# #         points(s.jantemp$YEAR,s.jantemp$TAVG,pch=19,col="dodgerblue4",cex=p.s)
#         lines(s.summer$YEAR,s.summer$WB,pch=18,col="orangered2",cex=p.s)
# #         points(s.julytemp$YEAR,s.julytemp$TAVG,pch=19,col="orangered4",cex=p.s)
#         abline(h=p.h,col="grey",lty=3)
#         dev.off()
#         
#       } #end streamtemp plot type
#       
#       
      
      
      
      
      
      
      
      
      
      
      
      


   }

}
