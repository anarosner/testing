#=================================================================================================================================
#This sets up the calculations of the partial derivatives of lambda wrt each demographic rates
#1) need to calc the partial deriviatives of K wrt each demographic rate
#2) Multiply this by the overall sensitivity of lambda to each entry in K yields the sensitivity of lambda to each demographic rate
#=================================================================================================================================



#Growth and recruit size are special
pdKyxGxyf<-function(x,y,flow,temp,season,river,params) {  
  stdX <- (x-params$lengthMean[season,river])/params$lengthSd[season,river]  
  
  var <- mean((params$grSigma[season,river,1:nIter])^2)
  sd <- var^(1/2)
  m  <- ( mean(params$grBeta[1,season,river,1:nIter]) +
    mean(params$grBeta[2,season,river,1:nIter])*stdX +
    mean(params$grBeta[3,season,river,1:nIter])*flow +
    mean(params$grBeta[4,season,river,1:nIter])*temp +
    mean(params$grBeta[5,season,river,1:nIter])*flow*temp +
    mean(params$grBeta[6,season,river,1:nIter])*flow*stdX +
    mean(params$grBeta[7,season,river,1:nIter])*temp*stdX +
    mean(params$grBeta[8,season,river,1:nIter])*flow*temp*stdX + x 
          )
  
  norm <- (1/(var^(1/2)*(2*pi)^(1/2))) * exp(-.5*(((y-m)^2)/var))
  
  u<-invlogit( mean(params$phiBeta[1,season,river,1:nIter]) +
    mean(params$phiBeta[2,season,river,1:nIter])*stdX +
    mean(params$phiBeta[3,season,river,1:nIter])*flow +
    mean(params$phiBeta[4,season,river,1:nIter])*temp +
    mean(params$phiBeta[5,season,river,1:nIter])*flow*temp +
    mean(params$phiBeta[6,season,river,1:nIter])*flow*stdX +
    mean(params$phiBeta[7,season,river,1:nIter])*temp*stdX +
    mean(params$phiBeta[8,season,river,1:nIter])*flow*temp*stdX
               )
   
  dgdm <- (y-m)/var * norm * u
  
  return(dgdm)
}


#Size of recruits . Need to work on. ONe thing to think about is that it is impossible to have recuits entering in all but season two. BUt should be taken care of if this function is zero.
pdKyxF2xyf <- function(x,y,season,river,params) {
  if(season == 2){
    
    
    
    m <- params$kidSizeMean[river]
    sd <- params$kidSizeSd[river]
    var <- sd^2
    
    norm <- (1/(var^(1/2)*(2*pi)^(1/2))) * exp(-.5*(((y-m)^2)/var))
    
    #probability of producing a seedling of size y
    #tmp<-dnorm(y,kidsize.mean,kidsize.sd)/(1-pnorm(0,kidsize.mean,kidsize.sd)) #what is this all about?
    #return(tmp)
    
    r = plogis(x, params$rInt, params$rSlope)
    nKids<-(params$nKidsSizeInt * x ^ params$NKidsSizeSlope)
    u <- params$earlySurv+0*x
    
    df2dm <- (y-m)/var * norm * u * r * nkids
    
    return(df2dm)
    
  }
  else{
    tmp <- x*0
  }
  return(tmp)
}


##### Put all component matrices into 3-dimensional arrays 
pdKyxGxy <-  array(NA,c(n.big.matrix,n.big.matrix,4,nRiver))

#F2xy <- array(NA,c(n.big.matrix,n.big.matrix,4,nRiver))



#Create  matrices, looping over age  
for (river in 1:nRiver) {
  for (season in 1:4){
    # Note the use of outer to compute each component matrix without looping.    
    #Note that h is only multiplied by demographic rates that are probability distributions (F2xy and Gxy)
 #   F2xy[,,season,river] <- h*t(outer(y,y,f2xy,season=season,river=river,params=pList))
    pdKyxGxy[,,season,river] <- h*t(outer(y,y,pdKyxGxyf,flow=flow,temp=temp,season=season,river=river,params=pList))
    
  }
}






###Partial derivatives of K wrt each demo rate
demoPd <- array(NA,c(6,length(y),length(y),4,nRiver))

#Construct the partial derivative kernels, which are the partial derivative of the the meta kernel K wrt each demographic rate 
#This could be done using the Jacobian, but the matrix is too large for R to handle
for (river in 1:nRiver) {
  for (season in 1:4){  
    demoPd[1,,,season,river] <-  pdKyxGxy[,,season,river]                                               #Growth
    demoPd[2,,,season,river] <- Gxy[,,season,river]                                               #Survival
    demoPd[3,,,season,river] <- SJx[,,season,river] * F2xy[,,season,river] * F3x[,,season,river]  #Number of offspring
    demoPd[4,,,season,river] <- SJx[,,season,river] * F1x[,,season,river] * F3x[,,season,river]   #Offspring size
    demoPd[5,,,season,river] <- SJx[,,season,river] * F2xy[,,season,river] * F1x[,,season,river]  #Probability of reproduction
    demoPd[6,,,season,river] <- F1x[,,season,river] * F2xy[,,season,river] * F3x[,,season,river]  #Offspring survival
  }
}



##To calculate the partial derivative of metaKMyear wrt to each demo param in each season
#Note that the elasticities will not sum to 1 or 4 and do not really give you the proportional contribution of each lower level trait
#Caswell p232
metaPdKyxVrrow <-       array(0,c(6,4,length(y),length(y)*nRiver))
metaPdKyxVr <-          array(0,c(6,4,length(y)*nRiver,length(y)*nRiver))  

for (rate in 1:6){
  for (season in 1:4){
    metaPdKyxVrrow[rate,season,,] <- abind(demoPd[rate,,,season,1],demoPd[rate,,,season,2],demoPd[rate,,,season,3],demoPd[rate,,,season,4], along=2)
    metaPdKyxVr[rate,season,,] <- abind( metaPdKyxVrrow[rate,season,,], metaPdKyxVrrow[rate,season,,], metaPdKyxVrrow[rate,season,,], metaPdKyxVrrow[rate,season,,],along=1)
    metaPdKyxVr[rate,season,,] <- metaPdKyxVr[rate,season,,] * diag(1,nRiver,nRiver) %x% array(1,c(length(y),length(y)))

  }
}



##To calculate the partial derivative of metaKMyear wrt to each demo param in each season
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

