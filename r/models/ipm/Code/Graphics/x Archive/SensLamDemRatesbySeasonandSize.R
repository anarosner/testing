pDemoSensSeasonSum <- array(NA,c(6,4,(length(y)+1)*nRiver))
for(rate in 1:6){
  for(season in 1:4){
    pDemoSensSeasonSum[rate,season,] <- colSums(pDemoSensSeason[rate,season,,])
  }
}


#To 1 From all

win.graph(); par(mfrow=c(4,4));
plot(ylong,pDemoSensSeasonSum[dRate,1,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Spring")
plot(ylong,pDemoSensSeasonSum[dRate,2,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Summer")
plot(ylong,pDemoSensSeasonSum[dRate,3,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Fall")
plot(ylong,pDemoSensSeasonSum[dRate,4,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Winter")

plot(ylong,pDemoSensSeasonSum[dRate,1,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Spring")
plot(ylong,pDemoSensSeasonSum[dRate,2,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Summer")
plot(ylong,pDemoSensSeasonSum[dRate,3,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Fall")
plot(ylong,pDemoSensSeasonSum[dRate,4,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Winter")

plot(ylong,pDemoSensSeasonSum[dRate,1,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Spring")
plot(ylong,pDemoSensSeasonSum[dRate,2,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Summer")
plot(ylong,pDemoSensSeasonSum[dRate,3,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Fall")
plot(ylong,pDemoSensSeasonSum[dRate,4,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Winter")

plot(ylong,pDemoSensSeasonSum[dRate,1,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Spring")
plot(ylong,pDemoSensSeasonSum[dRate,2,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Summer")
plot(ylong,pDemoSensSeasonSum[dRate,3,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Fall")
plot(ylong,pDemoSensSeasonSum[dRate,4,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Winter")




# To 2 from all
win.graph(); par(mfrow=c(4,4));
plot(ylong,pDemoSensSeasonSum[dRate,1,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Spring")
plot(ylong,pDemoSensSeasonSum[dRate,2,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Summer")
plot(ylong,pDemoSensSeasonSum[dRate,3,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Fall")
plot(ylong,pDemoSensSeasonSum[dRate,4,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Winter")

plot(ylong,pDemoSensSeasonSum[dRate,1,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Spring")
plot(ylong,pDemoSensSeasonSum[dRate,2,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Summer")
plot(ylong,pDemoSensSeasonSum[dRate,3,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Fall")
plot(ylong,pDemoSensSeasonSum[dRate,4,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Winter")

plot(ylong,pDemoSensSeasonSum[dRate,1,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Spring")
plot(ylong,pDemoSensSeasonSum[dRate,2,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Summer")
plot(ylong,pDemoSensSeasonSum[dRate,3,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Fall")
plot(ylong,pDemoSensSeasonSum[dRate,4,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Winter")

plot(ylong,pDemoSensSeasonSum[dRate,1,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Spring")
plot(ylong,pDemoSensSeasonSum[dRate,2,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Summer")
plot(ylong,pDemoSensSeasonSum[dRate,3,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Fall")
plot(ylong,pDemoSensSeasonSum[dRate,4,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Winter")



# To 3 from all
win.graph(); par(mfrow=c(4,4));
plot(ylong,pDemoSensSeasonSum[dRate,1,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Spring")
plot(ylong,pDemoSensSeasonSum[dRate,2,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Summer")
plot(ylong,pDemoSensSeasonSum[dRate,3,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Fall")
plot(ylong,pDemoSensSeasonSum[dRate,4,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Winter")

plot(ylong,pDemoSensSeasonSum[dRate,1,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Spring")
plot(ylong,pDemoSensSeasonSum[dRate,2,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Summer")
plot(ylong,pDemoSensSeasonSum[dRate,3,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Fall")
plot(ylong,pDemoSensSeasonSum[dRate,4,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Winter")

plot(ylong,pDemoSensSeasonSum[dRate,1,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Spring")
plot(ylong,pDemoSensSeasonSum[dRate,2,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Summer")
plot(ylong,pDemoSensSeasonSum[dRate,3,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Fall")
plot(ylong,pDemoSensSeasonSum[dRate,4,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Winter")

plot(ylong,pDemoSensSeasonSum[dRate,1,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Spring")
plot(ylong,pDemoSensSeasonSum[dRate,2,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Summer")
plot(ylong,pDemoSensSeasonSum[dRate,3,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Fall")
plot(ylong,pDemoSensSeasonSum[dRate,4,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Winter")



# To 4 from all
win.graph(); par(mfrow=c(4,4));
plot(ylong,pDemoSensSeasonSum[dRate,1,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Spring")
plot(ylong,pDemoSensSeasonSum[dRate,2,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Summer")
plot(ylong,pDemoSensSeasonSum[dRate,3,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Fall")
plot(ylong,pDemoSensSeasonSum[dRate,4,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Winter")

plot(ylong,pDemoSensSeasonSum[dRate,1,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Spring")
plot(ylong,pDemoSensSeasonSum[dRate,2,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Summer")
plot(ylong,pDemoSensSeasonSum[dRate,3,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Fall")
plot(ylong,pDemoSensSeasonSum[dRate,4,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Winter")

plot(ylong,pDemoSensSeasonSum[dRate,1,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Spring")
plot(ylong,pDemoSensSeasonSum[dRate,2,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Summer")
plot(ylong,pDemoSensSeasonSum[dRate,3,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Fall")
plot(ylong,pDemoSensSeasonSum[dRate,4,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Winter")

plot(ylong,pDemoSensSeasonSum[dRate,1,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Spring")
plot(ylong,pDemoSensSeasonSum[dRate,2,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Summer")
plot(ylong,pDemoSensSeasonSum[dRate,3,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Fall")
plot(ylong,pDemoSensSeasonSum[dRate,4,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Winter")