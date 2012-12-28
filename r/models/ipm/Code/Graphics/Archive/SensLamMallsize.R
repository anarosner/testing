
sumSensMSeason <- array(NA,c(4,nRiver,nRiver))
sensMSeasonXint <- array(NA,c(4,nRiver,nRiver,(length(y)+1)*nRiver))
sensMSeasonX <- array(NA,c(4,nRiver,nRiver,(length(y))))
for (season in 1:4){
  for (riverTo in 1:nRiver){
    for (riverFrom in 1:nRiver){
      p <- array(0,c(nRiver,nRiver))
      p[riverTo,riverFrom] <- 1
      p <- diag(1,(length(y)+1)) %x% p
      sumSensMSeason[season,riverTo,riverFrom] <- sum(brookAnalysis$MSensMain[season,,]*p)
      
#       sensMSeasonXint[season,riverTo,riverFrom,] <- colSums(brookAnalysis$MSensMain[season,,]*p)
#       sensMSeasonX[season,riverTo,riverFrom,] <- sensMSeasonXint[season,riverTo,riverFrom,sensMSeasonXint[season,riverTo,riverFrom,]!=0]
#       
    }
  }
}



#Movement all Sizes
win.graph(); par(mfrow=c(2,2));
barplot(sumSensMSeason[1,,],main="Spring",xlab="River From",names.arg=c(1,2,3,4),
        col=c("darkblue","red"),legend=colnames(sumSensMSeason),beside=TRUE)
barplot(sumSensMSeason[2,,],main="Summer",xlab="River From",names.arg=c(1,2,3,4),
        col=c("darkblue","red"),legend=colnames(sumSensMSeason),beside=TRUE)
barplot(sumSensMSeason[3,,],main="Fall",xlab="River From",names.arg=c(1,2,3,4),
        col=c("darkblue","red"),legend=colnames(sumSensMSeason),beside=TRUE)
barplot(sumSensMSeason[4,,],main="Winter",xlab="River From",names.arg=c(1,2,3,4),
        col=c("darkblue","red"),legend=colnames(sumSensMSeason),beside=TRUE)
