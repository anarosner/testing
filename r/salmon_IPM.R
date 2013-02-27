#!/usr/bin/env Rscript
print("starting salmon IPM script")

this_dir<-getwd()
source("./supporting_scripts/param_startup.R")
# source("./supporting_scripts/plot_functions.R")

#parse user input params
stocking_n <- parse.param("stocking_n")
stocking_stage <- parse.param("stocking_stage") 
#directories w/ preceding run outputs
flow_dir <- preceding$flow_dir
streamtemp_dir <- preceding$streamtemp_dir

print("starting ATS.R")
source(file = file.path(this_dir,"supporting_scripts/salmon_IPM/ATS.R"))
print("starting graphA.R")
with(data=list(run_dir=run_dir), expr = source(file = file.path(this_dir,"supporting_scripts/salmon_IPM/graphA.R"), local=TRUE))
print("starting graphB.R")
with(data=list(run_dir=run_dir), expr = source(file = file.path(this_dir,"supporting_scripts/salmon_IPM/graphB.R"), local=TRUE))
print("starting graphC.R")
with(data=list(run_dir=run_dir), expr = source(file = file.path(this_dir,"supporting_scripts/salmon_IPM/graphC.R"), local=TRUE))
print("starting graphD.R")
with(data=list(run_dir=run_dir), expr = source(file = file.path(this_dir,"supporting_scripts/salmon_IPM/graphD.R"), local=TRUE))




setwd(run_dir)
#graph a
a<-read.csv(file="graphA.csv",header=T)
a2<-a[a$stage_name=="two_spring_riverine",c("year", "total")]
a3<-a[a$stage_name=="three_spring_riverine",c("year", "total")]
a4<-a[a$stage_name=="four_spring_riverine",c("year", "total")]
#graph b
b<-read.csv("stock-graphB.csv")
b<-b[,c(2,4:7)]
names(b)<-c("year","bin1","bin2","bin3","bin4")
#graph c
c2<-read.csv("two_spring_riverine-graphC.csv")
c2<-c2[,c("X","X2030")]
names(c2)<-c("size","count")
c3<-read.csv("three_spring_riverine-graphC.csv")
c3<-c3[,c("X","X2030")]
names(c3)<-c("size","count")
c4<-read.csv("four_spring_riverine-graphC.csv")
c4<-c4[,c("X","X2030")]
names(c4)<-c("size","count")
#graph d
d0<-read.csv("zero_autumn_parr-graphD.csv")
d0<-d0[,c("X","X2030")]
names(d0)<-c("size","count")
d1<-read.csv("one_autumn_parr-graphD.csv")
d1<-d1[,c("X","X2030")]
names(d1)<-c("size","count")
d2<-read.csv("two_autumn_parr-graphD.csv")
d2<-d2[,c("X","X2030")]
names(d2)<-c("size","count")


#plot thumbnail 
p.h<-c(seq(from=0,to=max(c2$count),by=5))   
p.s<-0.7 #point/line scale
p.sl<-.7 #scale axis lines
svg(filename="thumbnail.svg",width=2.5,height=2.5)
  par(mai=c(.2,.2,.1,.1)) #svg (inches)
  par(fg="white",col.main="slategray4",cex=p.s,cex.main=p.s)
  par(family="serif")
  par(mgp=c(0,.1,0)) #margin b/w plot and axis labels
  plot(c2$size,c2$count,xlim=c(125,325),ylim=c(1,max(c2$count)),
    pch=18,col="sienna1",
    axes=F,xlab="",ylab="")
  points(c3$size,c3$count,col="sienna3")
  points(c4$size,c4$count,col="sienna4")
  axis(side=2,at=p.h,col="white", las=2,cex.axis=p.sl,col.axis="slategray4") 
  axis(side=1,col="white", las=1,cex.axis=p.sl,col.axis="slategray4") 
  abline(h=p.h,col="grey",lty=3)
dev.off()

# windowsFonts(
#   f3=windowsFont("Gill Sans MT")
# )
#plot a
p.h<-c(seq(from=0,to=max(a2$total),by=200))   
p.s<-0.7 #point/line scale
p.sl<-1.1 #scale axis lines
svg(filename="plot1.svg",width=8,height=6)
  par(mai=c(.5,.5,.4,.3)) #svg (inches)
  par(fg="white",col.main="steelblue4",cex=p.s,cex.main=1.8)
  par(family="serif")
  par(mgp=c(1.5,.1,0))
  par(cex.lab=1.2)
  plot(a2$year,a2$total,xlab="year",ylab="count",
       col="goldenrod1",pch=18,lty=2, type="b",
       axes=F,col.lab="slategray4")
  title(main="Number of Smolts Over Time - grouped by stage",family="f3")
  points(a3$year,a3$total,col="orange2",pch=18)
  points(a4$year,a4$total,col="orangered4",pch=18)
  axis(side=2,at=p.h,col="white", las=2,cex.axis=p.sl,col.axis="slategray4") 
  axis(side=1,col="white", las=1,cex.axis=p.sl,col.axis="slategray4") 
  abline(h=p.h,col="grey",lty=3)
dev.off()


#plot c 
p.h<-c(seq(from=0,to=max(c2$count),by=5))   
p.s<-0.7 #point/line scale
p.sl<-1.1 #scale axis lines
svg(filename="plot3.svg",width=8,height=6)
  par(mai=c(.5,.5,.4,.3)) #svg (inches)
  par(fg="white",col.main="steelblue4",cex=p.s,cex.main=1.8)
  par(family="serif")
  par(mgp=c(1.2,.1,0))
  par(cex.lab=1.2)
  
  plot(c2$size,c2$count,xlim=c(125,325),ylim=c(1,max(c2$count)),
       col="sienna1",pch=18,lty=2, type="b",
       axes=F,xlab="size",ylab="count",col.lab="slategray4")
  title(main="Size distribution of Salmon - grouped by stage",family="f3")
  points(c3$size,c3$count,col="sienna3",pch=18)
  lines(c3$size,c3$count,col="sienna3",lty=2)
  points(c4$size,c4$count,col="sienna4",pch=18)
  lines(c4$size,c4$count,col="sienna4",lty=2)
  axis(side=2,at=p.h,col="white", las=2,cex.axis=p.sl,col.axis="slategray4") 
  axis(side=1,col="white", las=1,cex.axis=p.sl,col.axis="slategray4") 
  abline(h=p.h,col="grey",lty=3)
dev.off()

#plot d 
p.h<-c(seq(from=0,to=max(d0$count),by=2))   
p.s<-0.7 #point/line scale
p.sl<-1.1 #scale axis lines
svg(filename="plot4.svg",width=8,height=6)
  par(mai=c(.5,.5,.4,.3)) #svg (inches)
  par(fg="white",col.main="steelblue4",cex=p.s,cex.main=1.8)
  par(family="serif")
  par(mgp=c(1.2,.1,0))
  par(cex.lab=1.2)
  plot(d0$size,d0$count,ylim=c(.25,max(d0$count)),xlab="size",ylab="count",
       col="sienna1",pch=18,lty=2, type="b",
       axes=F,col.lab="slategray4")
  title(main="Size distribution of Salmon - grouped by age",family="f3")
  points(d1$size,d1$count,col="sienna3",pch=18)
  lines(d1$size,d1$count,col="sienna3",lty=2)
  points(d2$size,d2$count,col="sienna4",pch=18)
  lines(d2$size,d2$count,col="sienna4",lty=2)
  axis(side=2,at=p.h,col="white", las=2,cex.axis=p.sl,col.axis="slategray4") 
  axis(side=1,col="white", las=1,cex.axis=p.sl,col.axis="slategray4") 
  abline(h=p.h,col="grey",lty=3)
dev.off()

