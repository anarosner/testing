#Plot Reproductive Value
win.graph(); par(mfrow=c(2,2));
plot(ylong,vectorsmetaKMyear[2,1,],type="p",xlab=("Length"),ylab=("Reproductive Value"));title("Spring")
plot(ylong,vectorsmetaKMyear[2,2,],type="p",xlab=("Length"),ylab=("Reproductive Value"));title("Summer")
plot(ylong,vectorsmetaKMyear[2,3,],type="p",xlab=("Length"),ylab=("Reproductive Value"));title("Fall")
plot(ylong,vectorsmetaKMyear[2,4,],type="p",xlab=("Length"),ylab=("Reproductive Value"));title("Winter")

#Plot Stable Size Distributions
win.graph(); par(mfrow=c(2,2));
plot(ylong,vectorsmetaKMyear[1,1,],type="p",xlab=("Length"),ylab=("Proportion"));title("Stable Size Spring")
plot(ylong,vectorsmetaKMyear[1,2,],type="p",xlab=("Length"),ylab=("Proportion"));title("Stable SizeSummer")
plot(ylong,vectorsmetaKMyear[1,3,],type="p",xlab=("Length"),ylab=("Proportion"));title("Stable SizeFall")
plot(ylong,vectorsmetaKMyear[1,4,],type="p",xlab=("Length"),ylab=("Proportion"));title("Stable SizeWinter")






y2 <- abind(y,y,y,y,along=1)

vectorsmetaKMyearA <- array(NA,c(2,4,length(y2)))
for(v in 1:2){
  for (season in 1:4){
    for (river in 1:4){
      a <- vectorsmetaKMyear[v,season,(river*(length(y)+1)-100):(river*(length(y)+1))]
      vectorsmetaKMyearA[v,season,(river*(length(y))-99):(river*(length(y)))] <- a[-1] 
    }
  }
}





#Plot Reproductive Value
win.graph(); par(mfrow=c(2,2));
plot(y2,vectorsmetaKMyearA[2,1,],type="p",xlab=("Length"),ylab=("Reproductive Value"));title("Spring")
plot(y2,vectorsmetaKMyearA[2,2,],type="p",xlab=("Length"),ylab=("Reproductive Value"));title("Summer")
plot(y2,vectorsmetaKMyearA[2,3,],type="p",xlab=("Length"),ylab=("Reproductive Value"));title("Fall")
plot(y2,vectorsmetaKMyearA[2,4,],type="p",xlab=("Length"),ylab=("Reproductive Value"));title("Winter")

#Plot Stable Size Distributions
win.graph(); par(mfrow=c(2,2));
plot(y2,vectorsmetaKMyearA[1,1,],type="p",xlab=("Length"),ylab=("Proportion"));title("Stable Size Spring")
plot(y2,vectorsmetaKMyearA[1,2,],type="p",xlab=("Length"),ylab=("Proportion"));title("Stable SizeSummer")
plot(y2,vectorsmetaKMyearA[1,3,],type="p",xlab=("Length"),ylab=("Proportion"));title("Stable SizeFall")
plot(y2,vectorsmetaKMyearA[1,4,],type="p",xlab=("Length"),ylab=("Proportion"));title("Stable SizeWinter")