

###### Probability of survival: 
pdsxtemp<-function(x,y,flow,temp,season,river,params) {
  
  stdX <- (x-params$lengthMean[season,river])/params$lengthSd[season,river] 
  
  u<-( mean(params$phiBeta[1,season,river,1:nIter]) +
    mean(params$phiBeta[2,season,river,1:nIter])*stdX +
    mean(params$phiBeta[3,season,river,1:nIter])*flow +
    mean(params$phiBeta[4,season,river,1:nIter])*temp +
    mean(params$phiBeta[5,season,river,1:nIter])*flow*temp +
    mean(params$phiBeta[6,season,river,1:nIter])*flow*stdX +
    mean(params$phiBeta[7,season,river,1:nIter])*temp*stdX +
    mean(params$phiBeta[8,season,river,1:nIter])*flow*temp*stdX)
  
  #keep all parameters with temp, but remove temp variable from equation
  dsdt <-
            ((exp(u)/(1+exp(u))) -  ((exp(u)^2)/((1+exp(u))^2)))*
            (
     #mean(params$phiBeta[2,season,river,1:nIter])*stdX +
    #mean(params$phiBeta[3,season,river,1:nIter])*flow +
           
    mean(params$phiBeta[4,season,river,1:nIter])#*temp 
    +
    mean(params$phiBeta[5,season,river,1:nIter])*flow#*temp 
    + 
          
    #mean(params$phiBeta[6,season,river,1:nIter])*stdX*flow +
    mean(params$phiBeta[7,season,river,1:nIter])*stdX#*temp 
           +
    mean(params$phiBeta[8,season,river,1:nIter])*stdX*flow#*temp
          )
           
  return(dsdt);
}




#Survival from egg to recruit
pdsjxtemp<-function(x,y,params) {
  
  u <- 0*x
  return(u);
}




#Growth probabilities
pdgxytemp<-function(x,y,flow,temp,season,river,params) {  
  stdX <- (x-params$lengthMean[season,river])/params$lengthSd[season,river]  
  
  dmdt  <- ( #mean(params$grBeta[1,season,river,1:nIter]) +
    #mean(params$grBeta[2,season,river,1:nIter])*stdX +
    #mean(params$grBeta[3,season,river,1:nIter])*flow + 
    
    mean(params$grBeta[4,season,river,1:nIter])#*temp 
    +
    mean(params$grBeta[5,season,river,1:nIter])*flow#*temp 
    +
    #mean(params$grBeta[6,season,river,1:nIter])*stdX*flow +
    mean(params$grBeta[7,season,river,1:nIter])*stdX#*temp 
    +
    mean(params$grBeta[8,season,river,1:nIter])*stdX*flow#*temp 
          )
  
  return(dmdt)
}

#Number of eggs
pdf1xtemp <- function(x,y,flow,temp,season,river,params) {
  
  
    nKids <- x*0
 
  return(nKids*0.5)   #The *0.5 is to account for the sex ratio. This can be included as another demographic rate if we want later
}


#Size of recruits
pdf2xytemp <- function(x,y,season,river,params) {

    tmp <- x*0

  return(tmp)
}


###### Probability of reproducing
pdf3xtemp <-function(x,y,season,river,params) {

    r <- x*0
 
  return(r)
}






# n.big.matrix is the number of mesh points for size, nRiver is the number of rivers 
n.big.matrix = 100; nRiver = pList$nRiver;

##### Put all component matrices into 3-dimensional arrays 
pdSxtemp <-   array(NA,c(n.big.matrix,n.big.matrix,4,nRiver))
pdSJxtemp <-  array(NA,c(n.big.matrix,n.big.matrix,4,nRiver))
pdGxytemp <-  array(NA,c(n.big.matrix,n.big.matrix,4,nRiver))
pdF1xtemp <-  array(NA,c(n.big.matrix,n.big.matrix,4,nRiver))
pdF2xytemp <- array(NA,c(n.big.matrix,n.big.matrix,4,nRiver))
pdF3xtemp <-  array(NA,c(n.big.matrix,n.big.matrix,4,nRiver))


# minimum and maximum sizes 
minsize= 40; maxsize=300; L= minsize; U= maxsize;
n = n.big.matrix 

# boundary points b and mesh points y
b = L+c(0:n)*(U-L)/n; 
y = 0.5*(b[1:n]+b[2:(n+1)]);

# step size for midpoint rule, see equations 4 and 5
h = y[2]-y[1]

#Create  matrices, looping over age  
for (river in 1:nRiver) {
  for (season in 1:4){
    # Note the use of outer to compute each component matrix without looping.    
    #Note that h is only multiplied by demographic rates that are probability distributions (F2xy and Gxy)
    pdF1xtemp[,,season,river] <-   t(outer(y,y,pdf1xtemp,flow=flow,temp=temp,season=season,river=river,params=pList))
    pdF2xytemp[,,season,river] <-  t(outer(y,y,pdf2xytemp,season=season,river=river,params=pList))
    pdF3xtemp[,,season,river] <-   t(outer(y,y,pdf3xtemp,season=season,river=river,params=pList))
    pdSxtemp[,,season,river] <-    t(outer(y,y,pdsxtemp,flow=flow,temp=temp,season=season,river=river,params=pList))  
    pdSJxtemp[,,season,river] <-   t(outer(y,y,pdsjxtemp,params=pList))
    pdGxytemp[,,season,river] <-   t(outer(y,y,pdgxytemp,flow=flow,temp=temp,season=season,river=river,params=pList))
    
  }
}


###combine
pdDemotemp <- array(NA,c(6,length(y),length(y),4,nRiver))


for (river in 1:nRiver) {
  for (season in 1:4){  
    pdDemotemp[1,,,season,river] <- pdGxytemp[,,season,river]       #Growth
    pdDemotemp[2,,,season,river] <- pdSxtemp[,,season,river]   #Survival
    pdDemotemp[3,,,season,river] <- pdF1xtemp[,,season,river]      #Number of offspring
    pdDemotemp[4,,,season,river] <- pdF2xytemp[,,season,river]   #Offspring size
    pdDemotemp[5,,,season,river] <- pdF3xtemp[,,season,river]  #Probability of reproduction
    pdDemotemp[6,,,season,river] <- pdSJxtemp[,,season,river]  #Offspring survival
  }
}

#This creates the meta matrix of demographic rates
metaPdDemotempRow <- array(0,c(6,4,length(y),length(y)*nRiver))
metaPdDemotemp <- array(0,c(6,4,length(y)*nRiver,length(y)*nRiver))  
for (rate in 1:6){
  for (season in 1:4){
    metaPdDemotempRow[rate,season,,] <- abind(pdDemotemp[rate,,,season,1],pdDemotemp[rate,,,season,2],pdDemotemp[rate,,,season,3],pdDemotemp[rate,,,season,4], along=2)
    metaPdDemotemp[rate,season,,] <- abind(  metaPdDemotempRow[rate,season,,],  metaPdDemotempRow[rate,season,,],  metaPdDemotempRow[rate,season,,],  metaPdDemotempRow[rate,season,,],along=1)
    metaPdDemotemp[rate,season,,] <- metaPdDemotemp[rate,season,,] * diag(1,nRiver,nRiver) %x% array(1,c(length(y),length(y)))
  }
}


