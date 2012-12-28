

sumSensMSeason <- array(NA,c(4,nRiver,nRiver))
sensMSeasonXint <- array(NA,c(4,nRiver,nRiver,(length(y)+1)*nRiver))
sensMSeasonX <- array(NA,c(4,nRiver,nRiver,(length(y)+1)))
for (season in 1:4){
  for (riverTo in 1:nRiver){
    for (riverFrom in 1:nRiver){
      p <- array(0,c(nRiver,nRiver))
      p[riverTo,riverFrom] <- 1
      p <- diag(1,(length(y)+1)) %x% p
      sumSensMSeason[season,riverTo,riverFrom] <- sum(MSensSeason[season,,]*p)
      sumSensMSeason[season,4,1:3] <- 0
      sensMSeasonXint[season,riverTo,riverFrom,] <- colSums(MSensSeason[season,,]*p)
      sensMSeasonX[season,riverTo,riverFrom,] <- abind(0,sensMSeasonXint[season,riverTo,riverFrom,sensMSeasonXint[season,riverTo,riverFrom,]!=0],along=1)
      sensMSeasonX[season,4,1:3,] <- 0 
    }
  }
}




#To 1 From all

win.graph(); par(mfrow=c(4,4));
plot(y,sensMSeasonX[1,1,1,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Spring")
plot(y,sensMSeasonX[2,1,2,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Summer")
plot(y,sensMSeasonX[3,1,3,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Fall")
plot(y,sensMSeasonX[4,1,4,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Winter")

plot(y,sensMSeasonX[1,1,1,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Spring")
plot(y,sensMSeasonX[2,1,2,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Summer")
plot(y,sensMSeasonX[3,1,3,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Fall")
plot(y,sensMSeasonX[4,1,4,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Winter")

plot(y,sensMSeasonX[1,1,1,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Spring")
plot(y,sensMSeasonX[2,1,2,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Summer")
plot(y,sensMSeasonX[3,1,3,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Fall")
plot(y,sensMSeasonX[4,1,4,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Winter")

plot(y,sensMSeasonX[1,1,1,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Spring")
plot(y,sensMSeasonX[2,1,2,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Summer")
plot(y,sensMSeasonX[3,1,3,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Fall")
plot(y,sensMSeasonX[4,1,4,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Winter")




# To 2 from all
win.graph(); par(mfrow=c(4,4));
plot(y,sensMSeasonX[1,2,1,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Spring")
plot(y,sensMSeasonX[2,2,2,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Summer")
plot(y,sensMSeasonX[3,2,3,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Fall")
plot(y,sensMSeasonX[4,2,4,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Winter")

plot(y,sensMSeasonX[1,2,1,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Spring")
plot(y,sensMSeasonX[2,2,2,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Summer")
plot(y,sensMSeasonX[3,2,3,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Fall")
plot(y,sensMSeasonX[4,2,4,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Winter")

plot(y,sensMSeasonX[1,2,1,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Spring")
plot(y,sensMSeasonX[2,2,2,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Summer")
plot(y,sensMSeasonX[3,2,3,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Fall")
plot(y,sensMSeasonX[4,2,4,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Winter")

plot(y,sensMSeasonX[1,2,1,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Spring")
plot(y,sensMSeasonX[2,2,2,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Summer")
plot(y,sensMSeasonX[3,2,3,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Fall")
plot(y,sensMSeasonX[4,2,4,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Winter")



# To 3 from all
win.graph(); par(mfrow=c(4,4));
plot(y,sensMSeasonX[1,3,1,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Spring")
plot(y,sensMSeasonX[2,3,2,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Summer")
plot(y,sensMSeasonX[3,3,3,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Fall")
plot(y,sensMSeasonX[4,3,4,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Winter")

plot(y,sensMSeasonX[1,3,1,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Spring")
plot(y,sensMSeasonX[2,3,2,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Summer")
plot(y,sensMSeasonX[3,3,3,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Fall")
plot(y,sensMSeasonX[4,3,4,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Winter")

plot(y,sensMSeasonX[1,3,1,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Spring")
plot(y,sensMSeasonX[2,3,2,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Summer")
plot(y,sensMSeasonX[3,3,3,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Fall")
plot(y,sensMSeasonX[4,3,4,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Winter")

plot(y,sensMSeasonX[1,3,1,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Spring")
plot(y,sensMSeasonX[2,3,2,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Summer")
plot(y,sensMSeasonX[3,3,3,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Fall")
plot(y,sensMSeasonX[4,3,4,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Winter")



# To 4 from all
win.graph(); par(mfrow=c(4,4));
plot(y,sensMSeasonX[1,4,1,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Spring")
plot(y,sensMSeasonX[2,4,2,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Summer")
plot(y,sensMSeasonX[3,4,3,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Fall")
plot(y,sensMSeasonX[4,4,4,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Winter")

plot(y,sensMSeasonX[1,4,1,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Spring")
plot(y,sensMSeasonX[2,4,2,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Summer")
plot(y,sensMSeasonX[3,4,3,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Fall")
plot(y,sensMSeasonX[4,4,4,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Winter")

plot(y,sensMSeasonX[1,4,1,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Spring")
plot(y,sensMSeasonX[2,4,2,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Summer")
plot(y,sensMSeasonX[3,4,3,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Fall")
plot(y,sensMSeasonX[4,4,4,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Winter")

plot(y,sensMSeasonX[1,4,1,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Spring")
plot(y,sensMSeasonX[2,4,2,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Summer")
plot(y,sensMSeasonX[3,4,3,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Fall")
plot(y,sensMSeasonX[4,4,4,],type="p",xlab=("Length"),ylab=("Sensitivity"));title("Winter")