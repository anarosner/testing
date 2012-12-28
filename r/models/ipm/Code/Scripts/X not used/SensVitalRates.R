#=================================================================================================================================
#This sets up the calculations of the partial derivatives of lambda wrt each demographic rate
#1) need to calc the partial deriviatives of K wrt each demographic rate
#2) Multiply this by the overall sensitivity of lambda to each entry in K yields the sensitivity of lambda to each demographic rate
#=================================================================================================================================

#Growth and recruit size are special
pdKyxGxyf<-function(x,y,flow,temp,season,river,params) {  
  type <- metaRiverA[river,2]
  stdX <- (x-params$lengthMean[season,river])/params$lengthSd[season,river]  
  
  var <- mean((params$grSigma[season,river,1:nIter])^2)
  sd <- var^(1/2)
  m  <- ( mean(params$grBeta[1,season,type,1:nIter]) +
    mean(params$grBeta[2,season,type,1:nIter])*stdX +
    mean(params$grBeta[3,season,type,1:nIter])* flow +
    mean(params$grBeta[4,season,type,1:nIter])*  temp+
    mean(params$grBeta[5,season,type,1:nIter])* flow*temp+
    mean(params$grBeta[6,season,type,1:nIter])*  flow*stdX+
    mean(params$grBeta[7,season,type,1:nIter])* temp*stdX+
    mean(params$grBeta[8,season,type,1:nIter])*   flow*temp*stdX + x
  )
  
  
  
  norm <- (1/(var^(1/2)*(2*pi)^(1/2))) * exp(-.5*(((y-m)^2)/var))
  
  
  dgdm <- (y-m)/var * norm 
  
  return(dgdm)
}


#Size of recruits.
pdKyxF2xyf <- function(x,y,season,river,params) {
  if(season == 2){
    type <- metaRiverA[river,2]
    m <- params$kidSizeMean[type]
    sd <- params$kidSizeSd[type]
    var <- sd^2
    
    norm <- (1/(var^(1/2)*(2*pi)^(1/2))) * exp(-.5*(((y-m)^2)/var))
    
    
    df2dm <- (y-m)/var * norm  
    
  }
  else{
    df2dm <- x*0
  }
  return(df2dm)
}


##### Put all component matrices into 3-dimensional arrays 
pdKyxGxy <-  array(NA,c(n.big.matrix,n.big.matrix,4,nRiver))
pdKyxF2xy <-  array(NA,c(n.big.matrix,n.big.matrix,4,nRiver))
#F2xy <- array(NA,c(n.big.matrix,n.big.matrix,4,nRiver))



#Create  matrices
for (river in 1:nRiver) {
  for (season in 1:4){
    
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
    
    
    # Note the use of outer to compute each component matrix without looping.    
    #Note that h is only multiplied by demographic rates that are probability distributions (F2xy and Gxy)
    pdKyxGxy[,,season,river] <- h*t(outer(y,y,pdKyxGxyf,flow=flow,temp=temp,season=season,river=river,params=pList))
    pdKyxF2xy[,,season,river] <- h*t(outer(y,y,pdKyxF2xyf,season=season,river=river,params=pList))
  }
}


##To calculate the partial derivative of metaKMyear wrt to each demo param in each season
###Partial derivatives of K wrt each demo rate
demoPd <- array(NA,c(6,length(y)+1,length(y)+1,4,nRiver))

#Growth
#Survival
#Number of offspring
#Offspring size
#Probability of reproduction
#Offspring survival 

for (season in 1:4){
  for (river in 1:nRiver){
    if (season == 1) {
      demoPd[1,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),Sx[,,season,river]*pdKyxGxy[,,season,river],along=1)),along=2)
      demoPd[2,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),Gxy[,,season,river],along=1)),along=2)
      demoPd[3,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),array(0,c(length(y),length(y))),along=1)),along=2)
      demoPd[4,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),array(0,c(length(y),length(y))),along=1)),along=2)
      demoPd[5,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),array(0,c(length(y),length(y))),along=1)),along=2)
      demoPd[6,,,season,river] <- abind((abind(1,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),array(0,c(length(y),length(y))),along=1)),along=2)  
    }
    if (season == 2) { 
      demoPd[1,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),Sx[,,season,river]*pdKyxGxy[,,season,river],along=1)),along=2)
      demoPd[2,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),Gxy[,,season,river],along=1)),along=2)
      demoPd[3,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),array(0,c(length(y),length(y))),along=1)),along=2)
      demoPd[4,,,season,river] <- abind((abind(0,SJx[1,1,season,river]*pdKyxF2xy[,1,season,river],along=1)),(abind(array(0,c(1,length(y))),array(0,c(length(y),length(y))),along=1)),along=2)
      demoPd[5,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),array(0,c(length(y),length(y))),along=1)),along=2)
      demoPd[6,,,season,river] <- abind((abind(0,F2xy[,1,season,river],along=1)),(abind(array(0,c(1,length(y))),array(0,c(length(y),length(y))),along=1)),along=2)  
    }
    if (season == 3) {
      demoPd[1,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),Sx[,,season,river]*pdKyxGxy[,,season,river],along=1)),along=2)
      demoPd[2,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind((F1x[1,,season,river]* F3x[1,,season,river]),Gxy[,,season,river],along=1)),along=2)
      demoPd[3,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind((Sx[1,,season,river] * F3x[1,,season,river]),array(0,c(length(y),length(y))),along=1)),along=2)
      demoPd[4,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),array(0,c(length(y),length(y))),along=1)),along=2)
      demoPd[5,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind((((Sx[1,,season,river])) * F1x[1,,season,river]),array(0,c(length(y),length(y))),along=1)),along=2)
      demoPd[6,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),array(0,c(length(y),length(y))),along=1)),along=2)  
    }
    if (season == 4) {
      demoPd[1,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),Sx[,,season,river]*pdKyxGxy[,,season,river],along=1)),along=2)
      demoPd[2,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),Gxy[,,season,river],along=1)),along=2)
      demoPd[3,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),array(0,c(length(y),length(y))),along=1)),along=2)
      demoPd[4,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),array(0,c(length(y),length(y))),along=1)),along=2)
      demoPd[5,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),array(0,c(length(y),length(y))),along=1)),along=2)
      demoPd[6,,,season,river] <- abind((abind(1,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),array(0,c(length(y),length(y))),along=1)),along=2)  
    } 
  }
}



metaPdKyxVr <- array(0,c(6,4,(length(y)+1)*nRiver,(length(y)+1)*nRiver))  
for (rate in 1:6){
  for (season in 1:4){
    for (river in 1:nRiver){
      metaPdKyxVr[rate,season,((river*(length(y)+1)-length(y)):(river*(length(y)+1))),((river*(length(y)+1)-length(y)):(river*(length(y)+1)))] <- demoPd[rate,,,season,river]
    }
  }
}


#Multiply each rate by the sensitivity of lambda to Kij.
#Note that the elasticities will not sum to 1 or 4 and do not really give you the proportional contribution of each lower level trait
#Caswell p232

pDemoSensSeason <- array(0,c(6,4,(length(y)+1)*nRiver,(length(y)+1)*nRiver)) 
pDemoElasSeason <- array(0,c(6,4,(length(y)+1)*nRiver,(length(y)+1)*nRiver))
for (rate in 1:6){
  for (season in 1:4){
    pDemoSensSeason[rate,season,,] <- metaPdKyxVr[rate,season,,]*KSensSeason[season,,]
    pDemoElasSeason[rate,season,,] <- (valuesmetaKMyear[season]^(-1)) * (metaDemo[rate,season,,]) * pDemoSensSeason[rate,season,,]
  }
}
