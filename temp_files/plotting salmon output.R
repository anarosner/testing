

#graph a
a<-read.csv(file="graphA.csv",header=T)
a[1:10,]
a2<-a[a$stage_name=="two_spring_riverine",c("year", "total")]
a3<-a[a$stage_name=="three_spring_riverine",c("year", "total")]
a4<-a[a$stage_name=="four_spring_riverine",c("year", "total")]

svg(filename="grapha.svg",width=8,height=6)
  plot(a2$year,a2$total,col="yellow",pch=16,main="Number of Smolts Over Time - grouped by stage",xlab="year",ylab="count")
  points(a3$year,a3$total,col="orange",pch=16)
  points(a4$year,a4$total,col="red",pch=16)
dev.off()

#graph b
b<-read.csv("stock-graphB.csv")
b<-b[,c(2,4:7)]
names(b)<-c("year","bin1","bin2","bin3","bin4")
b[1:10,]

svg(filename="graphb.svg",width=8,height=6)  
  plot(b$year,b$bin1,type="p",pch=16,col="yellow",ylim=c(min(b[,2:5]),max(b[,2:5])),main="Number of Smolts Over Time - grouped by size bin",ylab="count",xlab="year")
  points(b$year,b$bin2,pch=16,col="orange")
  points(b$year,b$bin3,pch=5,col="red")
  points(b$year,b$bin4,pch=5,col="brown")
dev.off()

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

# max(c2$size)
svg(filename="graphc.svg",width=8,height=6)
  plot(c2$size,c2$count,xlim=c(125,325),ylim=c(.9,max(c2$count)),main="Size distribution of Salmon - grouped by stage",xlab="size",ylab="count")
  points(c3$size,c3$count)
  points(c4$size,c4$count)
dev.off()

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

svg(filename="graphd.svg",width=8,height=6)
  plot(d0$size,d0$count,ylim=c(.25,max(d0$count)),main="Size distribution of Salmon - grouped by age",xlab="size",ylab="count")
  points(d1$size,d1$count)
  points(d2$size,d2$count)
dev.off()


