

###### Probability of survival: 
pdsxtef<-function(x,y,flow,temp,season,river,iter,params) {

  type <- metaRiverA[river,2]
  
  stdX <- (x-params$lengthMean[season,type])/params$lengthSd[season,type]
  u<-invlogit( (params$phiBeta[1,season,type,iter]) +
    (params$phiBeta[2,season,type,iter])*stdX +
    (params$phiBeta[3,season,type,iter])* flow +
    (params$phiBeta[4,season,type,iter])*  temp+
    (params$phiBeta[5,season,type,iter])* flow*temp+
    (params$phiBeta[6,season,type,iter])*  flow*stdX+
    (params$phiBeta[7,season,type,iter])* temp*stdX+
    (params$phiBeta[8,season,type,iter])*   flow*temp*stdX
               )
  dsdf <-
    ((exp(u)/(1+exp(u))) -  ((exp(u)^2)/((1+exp(u))^2)))*
    (#(params$phiBeta[1,season,type,iter]) +
    #(params$phiBeta[2,season,type,iter])*stdX +
    #(params$phiBeta[3,season,type,iter])* flow +
    (params$phiBeta[4,season,type,iter])*  +#temp+
    (params$phiBeta[5,season,type,iter])* flow+#*temp+
    #(params$phiBeta[6,season,type,iter])*  flow*stdX+
    (params$phiBeta[7,season,type,iter])*stdX+# temp
    (params$phiBeta[8,season,type,iter])*   flow*stdX+#*temp)
    
    (params$phiBeta[5,season,type,iter])+
    (params$phiBeta[8,season,type,iter])*stdX
    
    )
  return(dsdf);
}

pdsxflf<-function(x,y,flow,temp,season,river,iter,params) {
  
  type <- metaRiverA[river,2]
  
  stdX <- (x-params$lengthMean[season,type])/params$lengthSd[season,type]
  u<-invlogit( (params$phiBeta[1,season,type,iter]) +
    (params$phiBeta[2,season,type,iter])*stdX +
    (params$phiBeta[3,season,type,iter])* flow +
    (params$phiBeta[4,season,type,iter])*  temp+
    (params$phiBeta[5,season,type,iter])* flow*temp+
    (params$phiBeta[6,season,type,iter])*  flow*stdX+
    (params$phiBeta[7,season,type,iter])* temp*stdX+
    (params$phiBeta[8,season,type,iter])*   flow*temp*stdX
  )
  dsdf <-
    ((exp(u)/(1+exp(u))) -  ((exp(u)^2)/((1+exp(u))^2)))*
    (#(params$phiBeta[1,season,type,iter]) +
      #(params$phiBeta[2,season,type,iter])*stdX +
        (params$phiBeta[3,season,type,iter]) + #flow +
        #(params$phiBeta[4,season,type,iter])*  temp+
        (params$phiBeta[5,season,type,iter])*temp+# flow+
        (params$phiBeta[6,season,type,iter])*  stdX+#flow*+
        #(params$phiBeta[7,season,type,iter])* temp*stdX+
        (params$phiBeta[8,season,type,iter])*   temp*stdX+#*flow*)
        
        (params$phiBeta[5,season,type,iter])+
        (params$phiBeta[8,season,type,iter]))*stdX
      return(dsdf);
}


#Growth probabilities
pdgxytef<-function(x,y,flow,temp,season,river,iter,params) { 
  type <- metaRiverA[river,2]
 
 stdX <- (x-params$lengthMean[season,type])/params$lengthSd[season,type]  
  g  <- ( #mean(params$grBeta[1,season,type,iter]) +
    #mean(params$grBeta[2,season,type,iter])*stdX +
    #mean(params$grBeta[3,season,type,iter])* flow +
    mean(params$grBeta[4,season,type,iter])*  +#temp+
    mean(params$grBeta[5,season,type,iter])* flow+#*temp+
    #mean(params$grBeta[6,season,type,iter])*  flow*stdX+
    mean(params$grBeta[7,season,type,iter])* stdX+# temp*+
    mean(params$grBeta[8,season,type,iter])*   flow*stdX+#*temp
    
    mean(params$grBeta[5,season,type,iter])+
    mean(params$grBeta[5,season,type,iter])*stdX
          )
  var <- mean((params$grSigma[season,type,iter])^2)
  sd <- var^(1/2)
  
  return(dnorm(y,g+x,sd))
}

pdgxyflf<-function(x,y,flow,temp,season,river,iter,params) { 
  type <- metaRiverA[river,2]
  
  stdX <- (x-params$lengthMean[season,type])/params$lengthSd[season,type]  
  g  <- ( #mean(params$grBeta[1,season,type,iter]) +
    #mean(params$grBeta[2,season,type,iter])*stdX +
    mean(params$grBeta[3,season,type,iter])* +#flow +
    #mean(params$grBeta[4,season,type,iter])*  temp+
    mean(params$grBeta[5,season,type,iter])* temp+#flow*+
    mean(params$grBeta[6,season,type,iter])*  stdX+#flow
    #mean(params$grBeta[7,season,type,iter])* temp*stdX+
    mean(params$grBeta[8,season,type,iter])*   temp*stdX+#*flow
    
    mean(params$grBeta[5,season,type,iter])+
      mean(params$grBeta[5,season,type,iter])*stdX
  )
  var <- mean((params$grSigma[season,type,iter])^2)
  sd <- var^(1/2)
  
  return(dnorm(y,g+x,sd))
}



##### Put all component matrices into 3-dimensional arrays 



pdsxte <-  array(0,c(n.big.matrix,n.big.matrix,4,nRiver))
pdsxfl <-  array(0,c(n.big.matrix,n.big.matrix,4,nRiver))
pdgxyfl <-  array(NA,c(n.big.matrix,n.big.matrix,4,nRiver))
pdgxyte <-  array(NA,c(n.big.matrix,n.big.matrix,4,nRiver))

#Create  matrices
for (river in 1:nRiver) {
  for (season in 1:4){
    # Note the use of outer to compute each component matrix without looping.    
    #Note that h is only multiplied by demographic rates that are probability distributions (F2xy and Gxy)
    
    type <- metaRiverA[river,2]
    
    
    if (zeroCond==F){
      
        if (season==firstSeason){
          flow <- demoVariables[type,firstSeason,1]
          temp <- demoVariables[type,firstSeason,2]
        }
        if (season!=firstSeason){
          flow <- 0
          temp <- 0
        }
      
      
    }
    
    if (zeroCond==T){
      flow <- demoVariables[type,season,1]
      temp <- demoVariables[type,season,2]
    }
    
 
    pdgxyfl[,,season,river] <- h*t(outer(y,y,pdgxyflf,flow=flow,temp=temp,season=season,river=river,params=pList))
    pdgxyte[,,season,river] <- h*t(outer(y,y,pdgxytef,flow=flow,temp=temp,season=season,river=river,params=pList))
    
  }
}


if(means==F){
  for (river in 1:nRiver) {
    for (season in 1:4){
      
      pdsxfl[,,season,river] <-  t(outer(y,y,pdsxflf,flow=flow,temp=temp,season=season,river=river,iter=iter,params=pList)) 
      pdsxte[,,season,river] <-  t(outer(y,y,pdsxtef,flow=flow,temp=temp,season=season,river=river,iter=iter,params=pList))
    }
  }
}

if(means==T){
  for (river in 1:nRiver) {
    for (season in 1:4){
      for (i in 1:nIter){
        pdsxfl[,,season,river] <- pdsxfl[,,season,river]+ t(outer(y,y,pdsxflf,flow=flow,temp=temp,season=season,river=river,iter=i,params=pList)) 
        pdsxte[,,season,river] <- pdsxte[,,season,river]+ t(outer(y,y,pdsxtef,flow=flow,temp=temp,season=season,river=river,iter=i,params=pList))
      }
    }
  }
  
  for (river in 1:nRiver) {
    for (season in 1:4){
      pdsxfl[,,season,river] <- pdsxfl[,,season,river]/nIter
      pdsxte[,,season,river] <- pdsxte[,,season,river]/nIter
    }
  }
}



##To calculate the partial derivative of metaKMyear wrt to each demo param in each season
###Partial derivatives of K wrt each demo rate
fltePd <- array(NA,c(2,2,length(y)+1,length(y)+1,4,nRiver))

#Growth
#Survival
#Number of offspring
#Offspring size
#Probability of reproduction
#Offspring survival 

for (season in 1:4){
  for (river in 1:nRiver){
    if (season == 1) {
      fltePd[1,1,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),pdgxyfl[,,season,river]*pdKyxGxy[,,season,river],along=1)),along=2)
      fltePd[1,2,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),pdsxfl[,,season,river],along=1)),along=2)
      fltePd[2,1,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),pdgxyte[,,season,river]*pdKyxGxy[,,season,river],along=1)),along=2)
      fltePd[2,2,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),pdsxte[,,season,river],along=1)),along=2)
    }
    if (season == 2) { 
      fltePd[1,1,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),pdgxyfl[,,season,river]*pdKyxGxy[,,season,river],along=1)),along=2)
      fltePd[1,2,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),pdsxfl[,,season,river],along=1)),along=2)
      fltePd[2,1,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),pdgxyte[,,season,river]*pdKyxGxy[,,season,river],along=1)),along=2)
      fltePd[2,2,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),pdsxte[,,season,river],along=1)),along=2)
    }
    if (season == 3) {
      fltePd[1,1,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),pdgxyfl[,,season,river]*pdKyxGxy[,,season,river],along=1)),along=2)
      fltePd[1,2,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind((pdsxfl[1,,season,river]),pdsxfl[,,season,river],along=1)),along=2)
      fltePd[2,1,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),pdgxyte[,,season,river]*pdKyxGxy[,,season,river],along=1)),along=2)
      fltePd[2,2,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind((pdsxfl[1,,season,river]),pdsxte[,,season,river],along=1)),along=2)
    }
    if (season == 4) {
      fltePd[1,1,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),pdgxyfl[,,season,river]*pdKyxGxy[,,season,river],along=1)),along=2)
      fltePd[1,2,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),pdsxfl[,,season,river],along=1)),along=2)
      fltePd[2,1,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),pdgxyte[,,season,river]*pdKyxGxy[,,season,river],along=1)),along=2)
      fltePd[2,2,,,season,river] <- abind((abind(0,array(0,c(length(y),1)),along=1)),(abind(array(0,c(1,length(y))),pdsxte[,,season,river],along=1)),along=2)
    } 
  }
}



metaPdVrEnv <- array(0,c(2,2,4,(length(y)+1)*nRiver,(length(y)+1)*nRiver))  
for (rate in 1:2){
  for (season in 1:4){
    for (river in 1:nRiver){
      for (env in 1:2){
        metaPdVrEnv[env,rate,season,((river*(length(y)+1)-length(y)):(river*(length(y)+1))),((river*(length(y)+1)-length(y)):(river*(length(y)+1)))] <- fltePd[env,rate,,,season,river]
      }
    }
  }
}


#Multiply each rate by the sensitivity of lambda to Kij.
#Note that the elasticities will not sum to 1 or 4 and do not really give you the proportional contribution of each lower level trait
#Caswell p232

pEnvSensSeason <- array(0,c(2,2,4,(length(y)+1)*nRiver,(length(y)+1)*nRiver)) 
pEnvElasSeason <- array(0,c(2,2,4,(length(y)+1)*nRiver,(length(y)+1)*nRiver))
for (rate in 1:2){
  for (season in 1:4){
    for (env in 1:2){
      pEnvSensSeason[env,rate,season,,] <- metaPdKyxVr[rate,season,,]*KSensSeason[season,,]*metaPdVrEnv[env,rate,season,,]

    }
  }
}
