


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
    SJxint[,,season,river] <-   t(outer(y,y,sjx,river=river,params=pList))
    SJx[1,1,season,river] <-  SJxint[1,1,season,river]
   
  }
}

rm(F1xint)
rm(F3xint)
rm(F2xyint)
rm(SJxint)




#matrices of individual demographic rates
demo <- array(NA,c(6,length(y)+1,length(y)+1,4,nRiver))

#Growth
#Survival
#Number of offspring
#Offspring size
#Probability of reproduction
#Offspring survival 

for (season in 1:4){
  for (river in 1:nRiver){
    if (season == 1) {
      demo[1,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),Gxy[,,season,river],along=1)),along=2)
      demo[2,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),Sx[,,season,river],along=1)),along=2)
      demo[3,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),array(0,c(length(y),length(y))),along=1)),along=2)
      demo[4,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),array(0,c(length(y),length(y))),along=1)),along=2)
      demo[5,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),array(0,c(length(y),length(y))),along=1)),along=2)
      demo[6,,,season,river] <- abind((abind(SJx[1,1,season,river],array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),array(0,c(length(y),length(y))),along=1)),along=2)  
    }
    if (season == 2) { 
      demo[1,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),Gxy[,,season,river],along=1)),along=2)
      demo[2,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),Sx[,,season,river],along=1)),along=2)
      demo[3,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),array(0,c(length(y),length(y))),along=1)),along=2)
      demo[4,,,season,river] <- abind((abind(0,F2xy[,1,season,river],along=1)),(abind(array(0,c(1,length(y))),array(0,c(length(y),length(y))),along=1)),along=2)
      demo[5,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),array(0,c(length(y),length(y))),along=1)),along=2)
      demo[6,,,season,river] <- abind((abind(0,array(SJx[1,1,season,river],c(length(y))),along=1)),(abind(array(0,c(1,length(y))),array(0,c(length(y),length(y))),along=1)),along=2)  
    }
    if (season == 3) {
      demo[1,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),Gxy[,,season,river],along=1)),along=2)
      demo[2,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),Sx[,,season,river],along=1)),along=2)
      demo[3,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(F1x[,,season,river],array(0,c(length(y),length(y))),along=1)),along=2)
      demo[4,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),array(0,c(length(y),length(y))),along=1)),along=2)
      demo[5,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(F3x[,,season,river],array(0,c(length(y),length(y))),along=1)),along=2)
      demo[6,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array((SJx[1,1,season,river]^.5),c(length(y))),array(0,c(length(y),length(y))),along=1)),along=2)  
    }
    if (season == 4) {
      demo[1,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),Gxy[,,season,river],along=1)),along=2)
      demo[2,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),Sx[,,season,river],along=1)),along=2)
      demo[3,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),array(0,c(length(y),length(y))),along=1)),along=2)
      demo[4,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),array(0,c(length(y),length(y))),along=1)),along=2)
      demo[5,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),array(0,c(length(y),length(y))),along=1)),along=2)
      demo[6,,,season,river] <- abind((abind(SJx[1,1,season,river],array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),array(0,c(length(y),length(y))),along=1)),along=2)  
    } 
  }
}


#This creates the meta matrix of demographic rates
metaDemorow <- array(0,c(6,4,length(y)+1,(length(y)+1)*nRiver))
metaDemo <- array(0,c(6,4,(length(y)+1)*nRiver,(length(y)+1)*nRiver))  
for (rate in 1:6){
  for (season in 1:4){
    metaDemorow[rate,season,,] <- abind(demo[rate,,,season,1],demo[rate,,,season,2],demo[rate,,,season,3],demo[rate,,,season,4], along=2)
    metaDemo[rate,season,,] <- abind(  metaDemorow[rate,season,,],  metaDemorow[rate,season,,],  metaDemorow[rate,season,,],  metaDemorow[rate,season,,],along=1)
    metaDemo[rate,season,,] <- metaDemo[rate,season,,] * diag(1,nRiver,nRiver) %x% array(1,c(length(y)+1,length(y)+1))
  }
}




