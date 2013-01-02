
pDemoSensSeasonSum <- array(NA,c(4,nRiver))
pDemoElasSeasonSum <- array(NA,c(4,nRiver))
  for(season in 1:4){
    for (river in 1:nRiver){
      pDemoSensSeasonSum[season,river] <- sum(KSensSeason[season,((-99+100*river):(100*river)),((-99+100*river):(100*river))])
      pDemoElasSeasonSum[season,river] <- sum(KElasSeason[season,((-99+100*river):(100*river)),((-99+100*river):(100*river))])
  }
}



#Movement all Sizes
win.graph(); par(mfrow=c(2,2));
barplot(pDemoElasSeasonSum[1,],main="Spring",xlab="River",names.arg=c(1,2,3,4),
        col=c("darkblue","red"),legend=colnames(pDemoElasSeasonSum),beside=TRUE)
barplot(pDemoElasSeasonSum[2,],main="Summer",xlab="River",names.arg=c(1,2,3,4),
        col=c("darkblue","red"),legend=colnames(pDemoElasSeasonSum),beside=TRUE)
barplot(pDemoElasSeasonSum[3,],main="Fall",xlab="River",names.arg=c(1,2,3,4),
        col=c("darkblue","red"),legend=colnames(pDemoElasSeasonSum),beside=TRUE)
barplot(pDemoElasSeasonSum[4,],main="Winter",xlab="River",names.arg=c(1,2,3,4),
        col=c("darkblue","red"),legend=colnames(pDemoElasSeasonSum),beside=TRUE)


















win.graph(); par(mfrow=c(4,4));   #River across the top, season along the side
persp(y,y,KSensSeason[1,1:100,1:100], theta=60, zlim=c(0,.5), ticktype="detailed",r=10,
      xlab="size at time t+1", ylab="size at time t",zlab="Sensitivity Spring River 1"); 
persp(y,y,KSensSeason[1,101:200,101:200], theta=60, zlim=c(0,.5),ticktype="detailed",r=10,
      xlab="size at time t+1", ylab="size at time t",zlab="Sensitivity Spring River 2");
persp(y,y,KSensSeason[1,201:300,201:300], theta=60, zlim=c(0,.5),ticktype="detailed",r=10,
      xlab="size at time t+1", ylab="size at time t",zlab="Sensitivity Spring River 3");
persp(y,y,KSensSeason[1,301:400,301:400], theta=60, zlim=c(0,.5),ticktype="detailed",r=10,
      xlab="size at time t+1", ylab="size at time t",zlab="Sensitivity Spring River 4");

persp(y,y,KSensSeason[2,1:100,1:100], theta=60, zlim=c(0,.5), ticktype="detailed",r=10,
      xlab="size at time t+1", ylab="size at time t",zlab="Sensitivity Summer River 1"); 
persp(y,y,KSensSeason[2,101:200,101:200], theta=60, zlim=c(0,.5),ticktype="detailed",r=10,
      xlab="size at time t+1", ylab="size at time t",zlab="Sensitivity Summer River 2");
persp(y,y,KSensSeason[2,201:300,201:300], theta=60, zlim=c(0,.5),ticktype="detailed",r=10,
      xlab="size at time t+1", ylab="size at time t",zlab="Sensitivity Summer River 3");
persp(y,y,KSensSeason[2,301:400,301:400], theta=60, zlim=c(0,.5),ticktype="detailed",r=10,
      xlab="size at time t+1", ylab="size at time t",zlab="Sensitivity Summer River 4");

persp(y,y,KSensSeason[3,1:100,1:100], theta=60, zlim=c(0,.5), ticktype="detailed",r=10,
      xlab="size at time t+1", ylab="size at time t",zlab="Sensitivity Fall River 1"); 
persp(y,y,KSensSeason[3,101:200,101:200], theta=60, zlim=c(0,.5),ticktype="detailed",r=10,
      xlab="size at time t+1", ylab="size at time t",zlab="Sensitivity Fall River 2");
persp(y,y,KSensSeason[3,201:300,201:300], theta=60, zlim=c(0,.5),ticktype="detailed",r=10,
      xlab="size at time t+1", ylab="size at time t",zlab="Sensitivity Fall River 3");
persp(y,y,KSensSeason[3,301:400,301:400], theta=60, zlim=c(0,.5),ticktype="detailed",r=10,
      xlab="size at time t+1", ylab="size at time t",zlab="Sensitivity Fall River 4");

persp(y,y,KSensSeason[4,1:100,1:100], theta=60, zlim=c(0,.5), ticktype="detailed",r=10,
      xlab="size at time t+1", ylab="size at time t",zlab="Sensitivity Winter River 1"); 
persp(y,y,KSensSeason[4,101:200,101:200], theta=60, zlim=c(0,.5),ticktype="detailed",r=10,
      xlab="size at time t+1", ylab="size at time t",zlab="Sensitivity Winter River 2");
persp(y,y,KSensSeason[4,201:300,201:300], theta=60, zlim=c(0,.5),ticktype="detailed",r=10,
      xlab="size at time t+1", ylab="size at time t",zlab="Sensitivity Winter River 3");
persp(y,y,KSensSeason[4,301:400,301:400], theta=60, zlim=c(0,.5),ticktype="detailed",r=10,
      xlab="size at time t+1", ylab="size at time t",zlab="Sensitivity Winter River 4");
