


##### Put all component matrices into 3-dimensional arrays 
SJxint <-   array(NA,c(n.big.matrix,n.big.matrix,4,nRiver))
SJx <-   array(NA,c(eggSize,eggSize,4,nRiver))
Sx <-  array(NA,c(n.big.matrix,n.big.matrix,4,nRiver))
Gxy <-  array(NA,c(n.big.matrix,n.big.matrix,4,nRiver))
F1xint <-  array(NA,c(n.big.matrix,n.big.matrix,4,nRiver))
F1x <-  array(NA,c(eggSize,n.big.matrix,4,nRiver))
F3xint <-  array(NA,c(n.big.matrix,n.big.matrix,4,nRiver))
F3x <-  array(NA,c(eggSize,n.big.matrix,4,nRiver))
F2xyint <- array(NA,c(n.big.matrix,n.big.matrix,4,nRiver))
F2xy <- array(NA,c(n.big.matrix,eggSize,4,nRiver))


#Create  matrices
for (river in 1:nRiver) {
  for (season in 1:4){
    # Note the use of outer to compute each component matrix without looping.    
    #Note that h is only multiplied by demographic rates that are probability distributions (F2xy and Gxy)
    F1xint[,,season,river] <-   t(outer(y,y,f1x,flow=flow,temp=temp,season=season,river=river,params=pList))
    F1x[1,,season,river] <- F1xint[1,,season,river]
    F3xint[,,season,river] <-   t(outer(y,y,f3x,season=season,river=river,params=pList))
    F3x[1,,season,river] <-  F3xint[1,,season,river]
    F2xyint[,,season,river] <- h*t(outer(y,y,f2xy,season=season,river=river,params=pList))
    F2xy[,1,season,river] <- F2xyint[,1,season,river]
    Sx[,,season,river] <-   t(outer(y,y,sx,flow=flow,temp=temp,season=season,river=river,params=pList))  

    Gxy[,,season,river] <- h*t(outer(y,y,gxy,flow=flow,temp=temp,season=season,river=river,params=pList))
    SJxint[,,season,river] <-   t(outer(y,y,sjx,params=pList))
    SJx[1,1,season,river] <-  SJxint[1,1,season,river]
  }
}
rm(F1xint)
rm(F3xint)
rm(F2xyint)
rm(SJxint)

#matrices of individual demographic rates
demo <- array(NA,c(6,length(y),length(y),4,nRiver))

for (river in 1:nRiver) {
  for (season in 1:4){  
    demo[1,,,season,river] <-  Gxy[,,season,river]  #Growth
    demo[2,,,season,river] <-   Sx[,,season,river]  #Survival
    demo[3,,,season,river] <-  F1x[,,season,river]  #Number of offspring
    demo[4,,,season,river] <- F2xy[,,season,river]  #Offspring size
    demo[5,,,season,river] <-  F3x[,,season,river]  #Probability of reproduction
    demo[6,,,season,river] <-  SJx[,,season,river]  #Offspring survival
  }
}




#This creates the meta matrix of demographic rates
#metaDemorow <- array(0,c(6,4,length(y),length(y)*nRiver))
#metaDemo <- array(0,c(6,4,length(y)*nRiver,length(y)*nRiver))  
#for (rate in 1:6){
#  for (season in 1:4){
#    metaDemorow[rate,season,,] <- abind(demo[rate,,,season,1],demo[rate,,,season,2],demo[rate,,,season,3],demo[rate,,,season,4], along=2)
#    metaDemo[rate,season,,] <- abind(  metaDemorow[rate,season,,],  metaDemorow[rate,season,,],  metaDemorow[rate,season,,],  metaDemorow[rate,season,,],along=1)
#    metaDemo[rate,season,,] <- metaDemo[rate,season,,] * diag(1,nRiver,nRiver) %x% array(1,c(length(y),length(y)))
#  }
#}




