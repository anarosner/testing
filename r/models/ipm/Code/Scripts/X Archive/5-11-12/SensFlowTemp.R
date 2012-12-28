

###### Probability of survival: 
pdsxFxT<-function(x,y,flow,temp,season,river,params) {
  
  stdX <- (x-params$lengthMean[season,river])/params$lengthSd[season,river] 
  
  u<-( mean(params$phiBeta[1,season,river,1:nIter]) +
    mean(params$phiBeta[2,season,river,1:nIter])*stdX +
    mean(params$phiBeta[3,season,river,1:nIter])*flow +
    mean(params$phiBeta[4,season,river,1:nIter])*temp +
    mean(params$phiBeta[5,season,river,1:nIter])*flow*temp +
    mean(params$phiBeta[6,season,river,1:nIter])*flow*stdX +
    mean(params$phiBeta[7,season,river,1:nIter])*temp*stdX +
    mean(params$phiBeta[8,season,river,1:nIter])*flow*temp*stdX)
  
  #keep all parameters with flow, but remove flow variable from equation
  dsdf <-
            ((exp(u)/(1+exp(u))) -  ((exp(u)^2)/((1+exp(u))^2)))*
            (
     #mean(params$phiBeta[2,season,river,1:nIter])*stdX +
    #mean(params$phiBeta[3,season,river,1:nIter])*flow +
           
    #mean(params$phiBeta[4,season,river,1:nIter])*temp +
    mean(params$phiBeta[5,season,river,1:nIter])#*temp*flow 
           +
    #mean(params$phiBeta[6,season,river,1:nIter])*stdX*flow +
    #mean(params$phiBeta[7,season,river,1:nIter])*temp*stdX +
           
    mean(params$phiBeta[8,season,river,1:nIter])*stdX#*temp*flow
          )
           
  return(dsdf);
}




#Survival from egg to recruit
pdsjxFxT<-function(x,y,params) {
  
  u <- 0*x
  return(u);
}




#Growth probabilities
pdgxyFxT<-function(x,y,flow,temp,season,river,params) {  
  stdX <- (x-params$lengthMean[season,river])/params$lengthSd[season,river]  
  
  dmdf  <- ( #mean(params$grBeta[1,season,river,1:nIter]) +
    #mean(params$grBeta[2,season,river,1:nIter])*stdX +
    #mean(params$grBeta[3,season,river,1:nIter])*flow  +
   
    #mean(params$grBeta[4,season,river,1:nIter])*temp +
    mean(params$grBeta[5,season,river,1:nIter])#*temp*flow 
    +
    #mean(params$grBeta[6,season,river,1:nIter])*stdX*flow +
    #mean(params$grBeta[7,season,river,1:nIter])*temp*stdX +
    
    mean(params$grBeta[8,season,river,1:nIter])*temp*stdX#*flow 
          )
  
  return(dmdf)
}

#Number of eggs
pdf1xFxT <- function(x,y,flow,temp,season,river,params) {
  
  
    nKids <- x*0
 
  return(nKids*0.5)   #The *0.5 is to account for the sex ratio. This can be included as another demographic rate if we want later
}


#Size of recruits
pdf2xyFxT <- function(x,y,season,river,params) {

    tmp <- x*0

  return(tmp)
}


###### Probability of reproducing
pdf3xFxT<-function(x,y,season,river,params) {

    r <- x*0
 
  return(r)
}






# n.big.matrix is the number of mesh points for size, nRiver is the number of rivers 
n.big.matrix = 100; nRiver = pList$nRiver;

##### Put all component matrices into 3-dimensional arrays 
pdSxFxT <-   array(NA,c(n.big.matrix,n.big.matrix,4,nRiver))
pdSJxFxT <-  array(NA,c(n.big.matrix,n.big.matrix,4,nRiver))
pdGxyFxT <-  array(NA,c(n.big.matrix,n.big.matrix,4,nRiver))
pdF1xFxT <-  array(NA,c(n.big.matrix,n.big.matrix,4,nRiver))
pdF2xyFxT <- array(NA,c(n.big.matrix,n.big.matrix,4,nRiver))
pdF3xFxT <-  array(NA,c(n.big.matrix,n.big.matrix,4,nRiver))


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
    pdF1xFxT[,,season,river] <-   t(outer(y,y,pdf1xflow,flow=flow,temp=temp,season=season,river=river,params=pList))
    pdF2xyFxT[,,season,river] <-  t(outer(y,y,pdf2xyflow,season=season,river=river,params=pList))
    pdF3xFxT[,,season,river] <-   t(outer(y,y,pdf3xflow,season=season,river=river,params=pList))
    pdSxFxT[,,season,river] <-    t(outer(y,y,pdsxflow,flow=flow,temp=temp,season=season,river=river,params=pList))  
    pdSJxFxT[,,season,river] <-   t(outer(y,y,pdsjxflow,params=pList))
    pdGxyFxT[,,season,river] <-   t(outer(y,y,pdgxyflow,flow=flow,temp=temp,season=season,river=river,params=pList))
    
  }
}


###combine
pdDemoFxT <- array(NA,c(6,length(y),length(y),4,nRiver))


for (river in 1:nRiver) {
  for (season in 1:4){  
    pdDemoFxT[1,,,season,river] <- pdGxyFxT[,,season,river]       #Growth
    pdDemoFxT[2,,,season,river] <- pdSxFxT[,,season,river]   #Survival
    pdDemoFxT[3,,,season,river] <- pdF1xFxT[,,season,river]      #Number of offspring
    pdDemoFxT[4,,,season,river] <- pdF2xyFxT[,,season,river]   #Offspring size
    pdDemoFxT[5,,,season,river] <- pdF3xFxT[,,season,river]  #Probability of reproduction
    pdDemoFxT[6,,,season,river] <- pdSJxFxT[,,season,river]  #Offspring survival
  }
}

#This creates the meta matrix of demographic rates
metaPdDemoFxTRow <- array(0,c(6,4,length(y),length(y)*nRiver))
metaPdDemoFxT <- array(0,c(6,4,length(y)*nRiver,length(y)*nRiver))  
for (rate in 1:6){
  for (season in 1:4){
    metaPdDemoFxTRow[rate,season,,] <- abind(pdDemoFxT[rate,,,season,1],pdDemoFxT[rate,,,season,2],pdDemoFxT[rate,,,season,3],pdDemoFxT[rate,,,season,4], along=2)
    metaPdDemoFxT[rate,season,,] <- abind(  metaPdDemoFxTRow[rate,season,,],  metaPdDemoFxTRow[rate,season,,],  metaPdDemoFxTRow[rate,season,,],  metaPdDemoFxTRow[rate,season,,],along=1)
    metaPdDemoFxT[rate,season,,] <- metaPdDemoFxT[rate,season,,] * diag(1,nRiver,nRiver) %x% array(1,c(length(y),length(y)))
  }
}


