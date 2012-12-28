

###### Probability of survival: 
pdsxflow<-function(x,y,flow,temp,season,river,params) {
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
  
  u<-( mean(params$phiBeta[1,season,river,1:nIter]) +
    mean(params$phiBeta[2,season,river,1:nIter])*stdX +
    mean(params$phiBeta[3,season,river,1:nIter])*fl +
    mean(params$phiBeta[4,season,river,1:nIter])*te +
    mean(params$phiBeta[5,season,river,1:nIter])*fl*te +
    mean(params$phiBeta[6,season,river,1:nIter])*fl*stdX +
    mean(params$phiBeta[7,season,river,1:nIter])*te*stdX +
    mean(params$phiBeta[8,season,river,1:nIter])*fl*te*stdX)
  
  #keep all parameters with flow, but remove flow variable from equation
  dsdf <-
            ((exp(u)/(1+exp(u))) -  ((exp(u)^2)/((1+exp(u))^2)))*
            (
     #mean(params$phiBeta[2,season,river,1:nIter])*stdX +
    mean(params$phiBeta[3,season,river,1:nIter])#*flow 
           +
    #mean(params$phiBeta[4,season,river,1:nIter])*temp +
    mean(params$phiBeta[5,season,river,1:nIter])*te#*flow 
           +
    mean(params$phiBeta[6,season,river,1:nIter])*stdX#*flow +
    #mean(params$phiBeta[7,season,river,1:nIter])*temp*stdX 
           +
    mean(params$phiBeta[8,season,river,1:nIter])*te*stdX#*flow
          )
           
  return(dsdf);
}




#Survival from egg to recruit
pdsjxflow<-function(x,y,params) {
  
  u <- 0*x
  return(u);
}




#Growth probabilities
pdgxyflow<-function(x,y,flow,temp,season,river,params) {  

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
  
  dmdf  <- ( #mean(params$grBeta[1,season,river,1:nIter]) +
    #mean(params$grBeta[2,season,river,1:nIter])*stdX +
    mean(params$grBeta[3,season,river,1:nIter])#*flow 
    +
    #mean(params$grBeta[4,season,river,1:nIter])*temp +
    mean(params$grBeta[5,season,river,1:nIter])*te#*flow 
    +
    mean(params$grBeta[6,season,river,1:nIter])*stdX#*flow +
    #mean(params$grBeta[7,season,river,1:nIter])*temp*stdX 
    +
    mean(params$grBeta[8,season,river,1:nIter])*te*stdX#*flow 
          )
  
  return(dmdf)
}

#Number of eggs
pdf1xflow <- function(x,y,flow,temp,season,river,params) {
  
  
    nKids <- x*0
 
  return(nKids*0.5)   #The *0.5 is to account for the sex ratio. This can be included as another demographic rate if we want later
}


#Size of recruits
pdf2xyflow <- function(x,y,season,river,params) {

    tmp <- x*0

  return(tmp)
}


###### Probability of reproducing
pdf3xflow<-function(x,y,season,river,params) {

    r <- x*0
 
  return(r)
}







##### Put all component matrices into 3-dimensional arrays 
pdSxflow <-   array(NA,c(n.big.matrix,n.big.matrix,4,nRiver))
pdSJxflow <-  array(NA,c(n.big.matrix,n.big.matrix,4,nRiver))
pdGxyflow <-  array(NA,c(n.big.matrix,n.big.matrix,4,nRiver))
pdF1xflow <-  array(NA,c(n.big.matrix,n.big.matrix,4,nRiver))
pdF2xyflow <- array(NA,c(n.big.matrix,n.big.matrix,4,nRiver))
pdF3xflow <-  array(NA,c(n.big.matrix,n.big.matrix,4,nRiver))


#Create  matrices, looping over age  
for (river in 1:nRiver) {
  for (season in 1:4){
    # Note the use of outer to compute each component matrix without looping.    
    #Note that h is only multiplied by demographic rates that are probability distributions (F2xy and Gxy)
    pdF1xflow[,,season,river] <-   t(outer(y,y,pdf1xflow,flow=flow,temp=temp,season=season,river=river,params=pList))
    pdF2xyflow[,,season,river] <-  t(outer(y,y,pdf2xyflow,season=season,river=river,params=pList))
    pdF3xflow[,,season,river] <-   t(outer(y,y,pdf3xflow,season=season,river=river,params=pList))
    pdSxflow[,,season,river] <-    t(outer(y,y,pdsxflow,flow=flow,temp=temp,season=season,river=river,params=pList))  
    pdSJxflow[,,season,river] <-   t(outer(y,y,pdsjxflow,params=pList))
    pdGxyflow[,,season,river] <-   t(outer(y,y,pdgxyflow,flow=flow,temp=temp,season=season,river=river,params=pList))
    
  }
}


###combine
pdDemoFlow <- array(NA,c(6,length(y),length(y),4,nRiver))


for (river in 1:nRiver) {
  for (season in 1:4){  
    pdDemoFlow[1,,,season,river] <- pdGxyflow[,,season,river]       #Growth
    pdDemoFlow[2,,,season,river] <- pdSxflow[,,season,river]   #Survival
    pdDemoFlow[3,,,season,river] <- pdF1xflow[,,season,river]      #Number of offspring
    pdDemoFlow[4,,,season,river] <- pdF2xyflow[,,season,river]   #Offspring size
    pdDemoFlow[5,,,season,river] <- pdF3xflow[,,season,river]  #Probability of reproduction
    pdDemoFlow[6,,,season,river] <- pdSJxflow[,,season,river]  #Offspring survival
  }
}

#This creates the meta matrix of demographic rates
metaPdDemoFlowRow <- array(0,c(6,4,length(y),length(y)*nRiver))
metaPdDemoFlow <- array(0,c(6,4,length(y)*nRiver,length(y)*nRiver))  
for (rate in 1:6){
  for (season in 1:4){
    metaPdDemoFlowRow[rate,season,,] <- abind(pdDemoFlow[rate,,,season,1],pdDemoFlow[rate,,,season,2],pdDemoFlow[rate,,,season,3],pdDemoFlow[rate,,,season,4], along=2)
    metaPdDemoFlow[rate,season,,] <- abind(  metaPdDemoFlowRow[rate,season,,],  metaPdDemoFlowRow[rate,season,,],  metaPdDemoFlowRow[rate,season,,],  metaPdDemoFlowRow[rate,season,,],along=1)
    metaPdDemoFlow[rate,season,,] <- metaPdDemoFlow[rate,season,,] * diag(1,nRiver,nRiver) %x% array(1,c(length(y),length(y)))
  }
}


