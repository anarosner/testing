#=================================================================================================================================
#This sets up the calculations of the partial derivatives of lambda wrt each demographic rate
#1) need to calc the partial deriviatives of K wrt each demographic rate
#2) Multiply this by the overall sensitivity of lambda to each entry in K yields the sensitivity of lambda to each demographic rate
#=================================================================================================================================



#Growth and recruit size are special
pdKyxGxyf<-function(x,y,flow,temp,season,river,params) {  
  if (firstSeason == 0){
    fl  <- 0
    te <- 0
  }
  
  else {
    if (river==pertriver){  
      
      if (season == firstSeason){
        fl  <- flow
        te <- temp
      }
      else {
        fl <- 0
        te <- 0
      }
    }
    else {
      fl <- 0
      te <- 0
    }
  }
  stdX <- (x-params$lengthMean[season,river])/params$lengthSd[season,river]  
  
  var <- mean((params$grSigma[season,river,1:nIter])^2)
  sd <- var^(1/2)
  m  <- ( mean(params$grBeta[1,season,river,1:nIter]) +
    mean(params$grBeta[2,season,river,1:nIter])*stdX +
    mean(params$grBeta[3,season,river,1:nIter])*fl +
    mean(params$grBeta[4,season,river,1:nIter])*te +
    mean(params$grBeta[5,season,river,1:nIter])*fl*te +
    mean(params$grBeta[6,season,river,1:nIter])*fl*stdX +
    mean(params$grBeta[7,season,river,1:nIter])*te*stdX +
    mean(params$grBeta[8,season,river,1:nIter])*fl*te*stdX + x 
          )
  
  norm <- (1/(var^(1/2)*(2*pi)^(1/2))) * exp(-.5*(((y-m)^2)/var))
  
  u<-invlogit( mean(params$phiBeta[1,season,river,1:nIter]) +
    mean(params$phiBeta[2,season,river,1:nIter])*stdX +
    mean(params$phiBeta[3,season,river,1:nIter])*fl +
    mean(params$phiBeta[4,season,river,1:nIter])*te +
    mean(params$phiBeta[5,season,river,1:nIter])*fl*te +
    mean(params$phiBeta[6,season,river,1:nIter])*fl*stdX +
    mean(params$phiBeta[7,season,river,1:nIter])*te*stdX +
    mean(params$phiBeta[8,season,river,1:nIter])*fl*te*stdX
               )
   
  dgdm <- (y-m)/var * norm * u
  
  return(dgdm)
}


#Size of recruits.
pdKyxF2xyf <- function(x,y,season,river,params) {
  if(season == 2){
    
    m <- params$kidSizeMean[river]
    sd <- params$kidSizeSd[river]
    var <- sd^2
    
    norm <- (1/(var^(1/2)*(2*pi)^(1/2))) * exp(-.5*(((y-m)^2)/var))
    
    r = plogis(x, params$rInt, params$rSlope)
    nKids<-(params$nKidsSizeInt * x ^ params$NKidsSizeSlope)
    u <- params$earlySurv+0*x
    
    df2dm <- (y-m)/var * norm * u * r * nKids
    
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



#Create  matrices, looping over age  
for (river in 1:nRiver) {
  for (season in 1:4){
    # Note the use of outer to compute each component matrix without looping.    
    #Note that h is only multiplied by demographic rates that are probability distributions (F2xy and Gxy)
    pdKyxGxy[,,season,river] <- h*t(outer(y,y,pdKyxGxyf,flow=flow,temp=temp,season=season,river=river,params=pList))
    pdKyxF2xy[,,season,river] <- h*t(outer(y,y,pdKyxF2xyf,season=season,river=river,params=pList))
  }
}






###Partial derivatives of K wrt each demo rate
demoPd <- array(NA,c(6,length(y),length(y),4,nRiver))

#Construct the partial derivative kernels, which are the partial derivative of the the meta kernel K wrt each demographic rate 
#This could be done using the Jacobian, but the matrix is too large for R to handle
for (river in 1:nRiver) {
  for (season in 1:4){  
    demoPd[1,,,season,river] <- pdKyxGxy[,,season,river]                                          #Growth
    demoPd[2,,,season,river] <- Gxy[,,season,river]                                               #Survival
    demoPd[3,,,season,river] <- SJx[,,season,river] * F2xy[,,season,river] * F3x[,,season,river]  #Number of offspring
    demoPd[4,,,season,river] <- pdKyxF2xy[,,season,river]                                         #Offspring size
    demoPd[5,,,season,river] <- SJx[,,season,river] * F2xy[,,season,river] * F1x[,,season,river]  #Probability of reproduction
    demoPd[6,,,season,river] <- F1x[,,season,river] * F2xy[,,season,river] * F3x[,,season,river]  #Offspring survival
  }
}



##To calculate the partial derivative of metaKMyear wrt to each demo param in each season
metaPdKyxVrrow <-       array(0,c(6,4,length(y),length(y)*nRiver))
metaPdKyxVr <-          array(0,c(6,4,length(y)*nRiver,length(y)*nRiver))  

for (rate in 1:6){
  for (season in 1:4){
    metaPdKyxVrrow[rate,season,,] <- abind(demoPd[rate,,,season,1],demoPd[rate,,,season,2],demoPd[rate,,,season,3],demoPd[rate,,,season,4], along=2)
    metaPdKyxVr[rate,season,,] <- abind( metaPdKyxVrrow[rate,season,,], metaPdKyxVrrow[rate,season,,], metaPdKyxVrrow[rate,season,,], metaPdKyxVrrow[rate,season,,],along=1)
    metaPdKyxVr[rate,season,,] <- metaPdKyxVr[rate,season,,] * diag(1,nRiver,nRiver) %x% array(1,c(length(y),length(y)))

  }
}



#Multiply each rate by the sensitivity of lambda to Kij.
#Note that the elasticities will not sum to 1 or 4 and do not really give you the proportional contribution of each lower level trait
#Caswell p232

pDemoSensSeason <- array(0,c(6,4,length(y)*nRiver,length(y)*nRiver)) 
pDemoElasSeason <- array(0,c(6,4,length(y)*nRiver,length(y)*nRiver))
for (rate in 1:6){
  for (season in 1:4){
    pDemoSensSeason[rate,season,,] <- metaPdKyxVr[rate,season,,]*KSensSeason[season,,]
    pDemoElasSeason[rate,season,,] <- (valuesmetaKMyear[season]^(-1)) * (metaDemo[rate,season,,]) * pDemoSensSeason[rate,season,,]
  }
}
