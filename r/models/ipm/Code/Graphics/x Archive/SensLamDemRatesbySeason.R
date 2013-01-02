pDemoElasSeasonSum <- array(NA,c(nRiver,4,6))
for(rate in 1:6){
  for(season in 1:4){
    for (river in 1:nRiver){
      pDemoElasSeasonSum[river,season,rate] <- sum(pDemoElasSeason[rate,season,((-99+100*river):(100*river)),((-99+100*river):(100*river))])
    }
  }
}



#Movement all Sizes
win.graph(); par(mfrow=c(2,2));
barplot(pDemoElasSeasonSum[,1,],main="Spring",xlab="River",ylab="Elasticity",names.arg=c(1,2,3,4,5,6),
        col=c("darkblue","red"),legend=colnames(pDemoElasSeasonSum),beside=TRUE)
barplot(pDemoElasSeasonSum[,2,],main="Summer",xlab="River",ylab="Elasticity",names.arg=c(1,2,3,4,5,6),
        col=c("darkblue","red"),legend=colnames(pDemoElasSeasonSum),beside=TRUE)
barplot(pDemoElasSeasonSum[,3,],main="Fall",xlab="River",ylab="Elasticity",names.arg=c(1,2,3,4,5,6),
        col=c("darkblue","red"),legend=colnames(pDemoElasSeasonSum),beside=TRUE)
barplot(pDemoElasSeasonSum[,4,],main="Winter",xlab="River",ylab="Elasticity",names.arg=c(1,2,3,4,5,6),
        col=c("darkblue","red"),legend=colnames(pDemoElasSeasonSum),beside=TRUE)