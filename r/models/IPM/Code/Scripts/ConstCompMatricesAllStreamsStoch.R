


##### Put all component matrices into 3-dimensional arrays 
SJxint <-   array(NA,c(n.big.matrix,n.big.matrix,4,nRiver))
SJx <-   array(NA,c(eggSize,eggSize,4,nRiver))
Sxint <-  array(0,c(n.big.matrix,n.big.matrix,4,nRiver,nIter))
Sx <-  array(0,c(n.big.matrix,n.big.matrix,4,nRiver))
Gxy <-  array(NA,c(n.big.matrix,n.big.matrix,4,nRiver))

F1xint <-  array(NA,c(n.big.matrix,n.big.matrix,4,nRiver))
F1x <-  array(NA,c(eggSize,n.big.matrix,4,nRiver))
F3xint <-  array(0,c(n.big.matrix,n.big.matrix,4,nRiver))
F3x <-  array(0,c(eggSize,n.big.matrix,4,nRiver))
F2xyint <- array(0,c(n.big.matrix,n.big.matrix,4,nRiver))
F2xy <- array(0,c(n.big.matrix,eggSize,4,nRiver))


for (river in 1:nRiver) {
  # Note the use of outer to compute each component matrix without looping.    
  #Note that h is only multiplied by demographic rates that are probability distributions (F2xy and Gxy)
  
  type <- metaRiverA[river,2]
  #Note that seasons are rotated so that 3=1 and 4 =2 because iterations do not start in season 1 (Spring)
  
  flowS1 <- demoVariables[type,4,1]
  tempS1 <- demoVariables[type,4,2]
  flowS2 <- demoVariables[type,3,1]
  tempS2 <- demoVariables[type,3,2]
  flowS3 <- demoVariables[type,2,1]
  tempS3 <- demoVariables[type,2,2]
  flowS4 <- demoVariables[type,1,1]
  tempS4 <- demoVariables[type,1,2]
  
  F2xyint[,,2,river] <- h*t(outer(y,y,f2xy,flowS1=flowS1,flowS2=flowS2,flowS3=flowS3,flowS4=flowS4,tempS1=tempS1,tempS2=tempS2,tempS3=tempS3,tempS4=tempS4,river=river,params=pList))
  F2xy[,1,2,river] <- F2xyint[,1,2,river]
  for (season in 1:4){
    SJxint[,,season,river] <-   t(outer(y,y,sjx,river=river,flowS1=flowS1,flowS2=flowS2,flowS3=flowS3,flowS4=flowS4,tempS1=tempS1,tempS2=tempS2,tempS3=tempS3,tempS4=tempS4,params=pList))
    SJx[1,1,season,river] <-  SJxint[1,1,season,river]^(1/12)
    #       SJx[1,1,season,river] <- sJriver[river,yr]^(1/4)
    F3xint[,,3,river] <-   t(outer(y,y,f3x,flowS1=flowS1,flowS2=flowS2,flowS3=flowS3,flowS4=flowS4,tempS1=tempS1,tempS2=tempS2,tempS3=tempS3,tempS4=tempS4,river=river,season=3,params=pList))
    F3x[1,,3,river] <-  F3xint[1,,3,river]
  }
}






#Create  matrices
for (river in 1:nRiver) {
  for (season in 1:4){
    # Note the use of outer to compute each component matrix without looping.    
    #Note that h is only multiplied by demographic rates that are probability distributions (F2xy and Gxy)
    
    type <- metaRiverA[river,2]
    if (season == 1){s=3}
    if (season == 2){s=4}
    if (season == 3){s=1}
    if (season == 4){s=2}
    
            flow <- demoVariables[type,s,1]
            temp <- demoVariables[type,s,2]
  
    F1xint[,,season,river] <-   t(outer(y,y,f1x,flow=flow,temp=temp,season=season,river=river,params=pList))
    F1x[1,,season,river] <- F1xint[1,,season,river]

    Gxy[,,season,river] <- h*t(outer(y,y,gxy,flow=flow,temp=temp,season=season,river=river,params=pList))

  }
}

rm(F1xint)
rm(F3xint)
rm(F2xyint)
rm(SJxint)

if(means==F){
  for (river in 1:nRiver) {
    for (season in 1:4){
      type <- metaRiverA[river,2]
      if (season == 1){s=3}
      if (season == 2){s=4}
      if (season == 3){s=1}
      if (season == 4){s=2}
      
      flow <- demoVariables[type,s,1]
      temp <- demoVariables[type,s,2]
      Sx[,,season,river] <-  t(outer(y,y,sx,flow=flow,temp=temp,season=season,river=river,iter=iter,params=pList)) 
      
    }
  }
}

if(means==T){
  for (river in 1:nRiver) {
    for (season in 1:4){
      for (i in 1:nIter){
      type <- metaRiverA[river,2]
      if (season == 1){s=3}
      if (season == 2){s=4}
      if (season == 3){s=1}
      if (season == 4){s=2}
      
      flow <- demoVariables[type,s,1]
      temp <- demoVariables[type,s,2]
      Sxint[,,season,river,i] <-  t(outer(y,y,sx,flow=flow,temp=temp,season=season,river=river,iter=i,params=pList)) 
      Sx[,,season,river] <- Sx[,,season,river] + Sxint[,,season,river,i]
      }
      Sx[,,season,river] <- Sx[,,season,river]/nIter
    }
  }
}







