
if (Analytical==T){

  #This creates the function 'getEig', which is used to calculate w and v analytically. 
  getEigSens <- function(mat,transpose){
   if (pop==1){
     if(transpose==F) {vec=riverArea[season,,]%*%(Re(eigen(mat)$vectors[,1]));  };     #This calculates the right eigenvector
     if(transpose==T) {vec=solve(riverArea[season,,])%*%(Re(eigen(t(mat))$vectors[,1]));  };  #This calculates the left eigenvector #Note that this is not rescaled
   } 
   if (pop>1){
     if(transpose==F) {vec=Re(eigen(mat)$vectors[,1]);  };     #This calculates the right eigenvector
     if(transpose==T) {vec=Re(eigen(t(mat))$vectors[,1]);  };  #This calculates the left eigenvector #Note that this is not rescaled
   }

    
    
    return(vec)
  }


    #Calculate sensititivites of yearly metalambda to seasonal vital rates and movement 
    #Note that these sensitivity values are meaningless. Since they are complex combinations of demography and movement.
    #They are only needed to calculate the seasonal sensitivities below
    
    vw <-     array(0,c(2,4,(length(y)+1)*nRiver))
    KMSens <- array(0,c(4,(length(y)+1)*nRiver,(length(y)+1)*nRiver))
    KMElas <- array(0,c(4,(length(y)+1)*nRiver,(length(y)+1)*nRiver))
    vDotW <-  array(0,c(4,(length(y)+1)*nRiver))
    
    for (season in 1:4){
      f <- c(season, season : (season + 2) %% 4 + 1)

        vw[1,season,] <- getEigSens(metaKMyear[season,,],F)  
        vw[2,season,] <- getEigSens(metaKMyear[season,,],T)  

      KMSens[season,,] <- outer(vw[2,season,],vw[1,season,])
      vDotW[season,] <-     sum(vw[2,season,]*vw[1,season,])
      KMSens[season,,] <- KMSens[season,,]/vDotW[season,]
      KMElas[season,,] <- (valuesmetaKMyear[season]^(-1)) * (metaKMyear[season,,]) * KMSens[season,,]
     
    }

}

if (Analytical==F){
  vw <-     array(0,c(2,4,(length(y)+1)*nRiver))
  KMSens <- array(0,c(4,(length(y)+1)*nRiver,(length(y)+1)*nRiver))
  KMElas <- array(0,c(4,(length(y)+1)*nRiver,(length(y)+1)*nRiver))
  vDotW <-  array(0,c(4,(length(y)+1)*nRiver))
  for (season in 1:4){
    f <- c(season, season : (season + 2) %% 4 + 1)
      KMSens[season,,] <- outer(vectorsmetaKMyear[2,season,],vectorsmetaKMyear[1,season,])
      vDotW[season,] <-     sum(vectorsmetaKMyear[2,season,]*vectorsmetaKMyear[1,season,])
      KMSens[season,,] <- KMSens[season,,]/vDotW[season,]
      KMElas[season,,] <- (valuesmetaKMyear[season]^(-1)) * (metaKMyear[season,,]) * KMSens[season,,] 

  }
}


#Creates the possible Sensitivity elements
possSens <- array(NA,c(length(y)+1,length(y)+1,4,nRiver))
for (season in 1:4){
  for (river in 1:nRiver){
    if (season == 1) {
      possSens[,,season,river] <- abind((abind(1,array(0,c(length(y))),along=1)),(abind(array(0,c(length(y))),array(1,c(length(y),length(y))),along=1)),along=2)  
    }
    if (season == 2){
      possSens[,,season,river] <- abind((abind(0,array(1,c(length(y))),along=1)),(abind(array(0,c(length(y))),array(1,c(length(y),length(y))),along=1)),along=2)
    }  
    if (season == 3){
      possSens[,,season,river] <- abind((abind(0,array(0,c(length(y))),along=1)),(abind(array(1,c(length(y))),array(1,c(length(y),length(y))),along=1)),along=2)
    }
    if (season == 4) {
      possSens[,,season,river] <- abind((abind(1,array(0,c(length(y))),along=1)),(abind(array(0,c(length(y))),array(1,c(length(y),length(y))),along=1)),along=2)
    }
  }
}

#This puts them into a meta matrix. 

metapossSens <- array(0,c(4,(length(y)+1)*nRiver,(length(y)+1)*nRiver))  #This is the whole possSens for a season 


for (season in 1:4){
  for (river in 1:nRiver){
    metapossSens[season,((river*(length(y)+1)-length(y)):(river*(length(y)+1))),((river*(length(y)+1)-length(y)):(river*(length(y)+1)))] <- possSens[,,season,river]
  }
}


#The following code calculates the sensitivity of lambda to either season migration or demography. Not both together.
#Calculate sensitivities of yearly metalambda to seasonal vital rates
KSensSeason <- array(0,c(4,(length(y)+1)*nRiver,(length(y)+1)*nRiver))
KElasSeason <- array(0,c(4,(length(y)+1)*nRiver,(length(y)+1)*nRiver))
for (season in 1:4){
  f <- c(season, season : (season + 2) %% 4 + 1)
  if (pop==1){
    KSensSeason[season,,] <-t(riverArea[season,,]%*%t(perm)%*%(metaM[f[4],,])%*%perm%*%metaK[f[4],,]%*%
      t(perm)%*%(metaM[f[3],,])%*%perm%*%metaK[f[3],,]%*%
      t(perm)%*%(metaM[f[2],,])%*%perm%*%metaK[f[2],,]%*%
      t(perm)%*%(metaM[f[1],,])%*%perm)%*%
      KMSens[f[1],,]%*%t(solve(riverArea[season,,]))
  }  
  
  if (pop>1){
  KSensSeason[season,,] <-t(t(perm)%*%metaM[f[4],,]%*%perm%*%metaK[f[4],,]%*%
    t(perm)%*%metaM[f[3],,]%*%perm%*%metaK[f[3],,]%*%
    t(perm)%*%metaM[f[2],,]%*%perm%*%metaK[f[2],,]%*%
    t(perm)%*%metaM[f[1],,]%*%perm)%*%
    KMSens[f[1],,]
  }
  KElasSeason[season,,] <- (valuesmetaKMyear[season]^(-1)) * (metaK[season,,]) * KSensSeason[season,,]
  
  KSensSeason[season,,] <- KSensSeason[season,,] * metapossSens[season,,]
  KElasSeason[season,,] <- KElasSeason[season,,] * metapossSens[season,,]
  
  
  
  }

sum(KElasSeason) #Should equal nRiver



#Calculate sensitivities of yearly metalambda to seasonal movement
MSensSeason <- array(0,c(4,(length(y)+1)*nRiver,(length(y)+1)*nRiver))
MElasSeason <- array(0,c(4,(length(y)+1)*nRiver,(length(y)+1)*nRiver))
for (season in 1:4){
  f <- c(season, season : (season + 2) %% 4 + 1)
  if (pop==1){
  MSensSeason[season,,] <-t(riverArea[season,,]%*%t(perm) %*% metaM[f[4],,] %*% perm %*% metaK[f[4],,] %*% 
                            t(perm) %*% metaM[f[3],,] %*% perm %*% metaK[f[3],,] %*% 
                            t(perm) %*% metaM[f[2],,] %*% perm %*% metaK[f[2],,] %*% 
                            t(perm)) %*% KMSens[f[1],,] %*% t(perm %*% metaK[f[1],,]%*%solve(riverArea[season,,]))   
  }
  if (pop>1){
  MSensSeason[season,,] <-t(t(perm) %*% metaM[f[4],,] %*% perm %*% metaK[f[4],,] %*% 
    t(perm) %*% metaM[f[3],,] %*% perm %*% metaK[f[3],,] %*% 
    t(perm) %*% metaM[f[2],,] %*% perm %*% metaK[f[2],,] %*% 
    t(perm)) %*% KMSens[f[1],,] %*% t(perm %*% metaK[f[1],,])
  }
  
  MElasSeason[season,,] <- (valuesmetaKMyear[season]^(-1)) * (metaM[season,,]) * MSensSeason[season,,]
  
  MSensSeason[season,,] <- MSensSeason[season,,] * diag(1,(length(y)+1)) %x% array(1,c(nRiver,nRiver))
  MElasSeason[season,,] <- MElasSeason[season,,] * diag(1,(length(y)+1)) %x% array(1,c(nRiver,nRiver))
  }

sum(MElasSeason) #Should equal nRiver


#Note that the above sensitivity and elasticity to movement are not really what we want since changing the prob of dispersing 
#to one river should be linked to a changed probability somehwere else. The way to account for this is to treat each 
#prob as a lower level parameter. This is below.
#pg19 Hunter and Caswell. For our case, it is in my (Ron) notebook pgs19-20.


#Note this will have to change if we do not allow individuals to move into river 4.
MSensSeason1 <- array(0,c(4,(length(y)+1)*nRiver,(length(y)+1)*nRiver))

for (season in 1:4){
  sumCol <- -colSums(MSensSeason[season,,]) ##The negative in front of colSums is suppose to be there
  for (mRow in 1:((length(y)+1)*nRiver)){
    MSensSeason1[season,mRow,] <- sumCol + 2*MSensSeason[season,mRow,]  
  }
  rm(sumCol)
  MSensSeason[season,,] <- MSensSeason1[season,,]*diag(1,(length(y)+1)) %x% array(1,c(nRiver,nRiver))
  MElasSeason[season,,] <- (valuesmetaKMyear[season]^(-1)) * (metaM[season,,]) * MSensSeason[season,,] 
  }

sum(MElasSeason)    #Note that these will not sum to 1 or 4, because they include the covariance between migration probabilites#Caswellp232 
