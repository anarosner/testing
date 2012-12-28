


##### Put all component matrices into 3-dimensional arrays 
SJxint <-   array(NA,c(n.big.matrix,n.big.matrix,4,nRiver))
SJx <-   array(NA,c(eggSize,eggSize,4,nRiver))

F1xint <-  array(NA,c(n.big.matrix,n.big.matrix,4,nRiver))
F1x <-  array(NA,c(eggSize,n.big.matrix,4,nRiver))
F3xint <-  array(NA,c(n.big.matrix,n.big.matrix,4,nRiver))
F3x <-  array(NA,c(eggSize,n.big.matrix,4,nRiver))
F2xyint <- array(0,c(n.big.matrix,n.big.matrix,4,nRiver))
F2xy <- array(0,c(n.big.matrix,eggSize,4,nRiver))
Sxint <-  array(0,c(n.big.matrix,n.big.matrix,4,nRiver,nIter))
Sx <-  array(0,c(n.big.matrix,n.big.matrix,4,nRiver))
Gxy <-  array(NA,c(n.big.matrix,n.big.matrix,4,nRiver))

#Create  matrices
for (river in 1:nRiver) {
  for (season in 1:4){
    # Note the use of outer to compute each component matrix without looping.    
    #Note that h is only multiplied by demographic rates that are probability distributions (F2xy and Gxy)
    
    type <- metaRiverA[river,2]
    
    
    if (zeroCond==F){
      if (type==pertriver){
        if (season==firstSeason){
          flow <- demoVariables[pertriver,firstSeason,1]
          temp <- demoVariables[pertriver,firstSeason,2]
        }
        if (season!=firstSeason){
          flow <- 0
          temp <- 0
        }
      }
      if (type!=pertriver){
        
        flow <- 0
        temp <- 0
        
      }
    }
    
    if (zeroCond==T){
      flow <- demoVariables[type,season,1]
      temp <- demoVariables[type,season,2]
    }
    
    
    F1xint[,,season,river] <-   t(outer(y,y,f1x,flow=flow,temp=temp,season=season,river=river,params=pList))
    F1x[1,,season,river] <- F1xint[1,,season,river]
    
    
    Gxy[,,season,river] <- h*t(outer(y,y,gxy,flow=flow,temp=temp,season=season,river=river,iter=iter,params=pList))
    
    
  }
}




for (river in 1:nRiver) {
  
  # Note the use of outer to compute each component matrix without looping.    
  #Note that h is only multiplied by demographic rates that are probability distributions (F2xy and Gxy)
  
  type <- metaRiverA[river,2]
  
  
  if (zeroCond==F){
    if (type==pertriver){
      if (firstSeason==1){
        flowS1 <- demoVariables[pertriver,2,1]
        tempS1 <- demoVariables[pertriver,2,2]
      }
      if (firstSeason==2){
        flowS2 <- demoVariables[pertriver,1,1]
        tempS2 <- demoVariables[pertriver,1,2]
      }
      if (firstSeason==3){
        flowS3 <- demoVariables[pertriver,4,1]
        tempS3 <- demoVariables[pertriver,4,2]
      }
      
    }
    if (type!=pertriver){
      
      flowS1 <- 0
      tempS1 <- 0
      flowS2 <- 0
      tempS2 <- 0
      flowS3 <- 0
      tempS3 <- 0
      flowS4 <- 0
      tempS4 <- 0
    }
  }
  
  if (zeroCond==T){
    flowS1 <- demoVariables[type,2,1]
    tempS1 <- demoVariables[type,2,2]
    flowS2 <- demoVariables[type,1,1]
    tempS2 <- demoVariables[type,1,2]
    flowS3 <- demoVariables[type,4,1]
    tempS3 <- demoVariables[type,4,2]
    flowS4 <- demoVariables[type,3,1]
    tempS4 <- demoVariables[type,3,2]
  }
  
  F2xyint[,,2,river] <- h*t(outer(y,y,f2xy,flowS1=flowS1,flowS2=flowS2,flowS3=flowS3,flowS4=flowS4,tempS1=tempS1,tempS2=tempS2,tempS3=tempS3,tempS4=tempS4,river=river,params=pList))
  F2xy[,1,2,river] <- F2xyint[,1,2,river]
  F3xint[,,3,river] <-   t(outer(y,y,f3x,flowS1=flowS1,flowS2=flowS2,flowS3=flowS3,flowS4=flowS4,tempS1=tempS1,tempS2=tempS2,tempS3=tempS3,tempS4=tempS4,river=river,season=3,params=pList))
  F3x[1,,3,river] <-  F3xint[1,,3,river]
  for (season in 1:4){
    SJxint[,,season,river] <-   t(outer(y,y,sjx,river=river,flowS1=flowS1,flowS2=flowS2,flowS3=flowS3,flowS4=flowS4,tempS1=tempS1,tempS2=tempS2,tempS3=tempS3,tempS4=tempS4,params=pList))
    SJx[1,1,season,river] <-  SJxint[1,1,season,river]^(1/12)
    
  }
  
}





rm(F1xint)
rm(F3xint)
rm(F2xyint)
rm(SJxint)

if(means==F){
  if (zeroCond==F){
    if (type==pertriver){
      if (season==firstSeason){
        flow <- demoVariables[pertriver,firstSeason,1]
        temp <- demoVariables[pertriver,firstSeason,2]
      }
      if (season!=firstSeason){
        flow <- 0
        temp <- 0
      }
    }
    if (type!=pertriver){
      
      flow <- 0
      temp <- 0
      
    }
  }
  
  if (zeroCond==T){
    flow <- demoVariables[type,season,1]
    temp <- demoVariables[type,season,2]
  }
  
  for (river in 1:nRiver) {
    for (season in 1:4){
      
      Sx[,,season,river] <-  t(outer(y,y,sx,flow=flow,temp=temp,season=season,river=river,iter=iter,params=pList)) 
      
    }
  }
}

if(means==T){
  if (zeroCond==F){
    if (type==pertriver){
      if (season==firstSeason){
        flow <- demoVariables[pertriver,firstSeason,1]
        temp <- demoVariables[pertriver,firstSeason,2]
      }
      if (season!=firstSeason){
        flow <- 0
        temp <- 0
      }
    }
    if (type!=pertriver){
      
      flow <- 0
      temp <- 0
      
    }
  }
  
  if (zeroCond==T){
    flow <- demoVariables[type,season,1]
    temp <- demoVariables[type,season,2]
  }
  
  for (river in 1:nRiver) {
    for (season in 1:4){
      for (i in 1:nIter){
        type <- metaRiverA[river,2]
        Sxint[,,season,river,i] <-  t(outer(y,y,sx,flow=flow,temp=temp,season=season,river=river,iter=i,params=pList)) 
        Sx[,,season,river] <- Sx[,,season,river] + Sxint[,,season,river,i]
      }
      Sx[,,season,river] <- Sx[,,season,river]/nIter
    }
  }
}


