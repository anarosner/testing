


#This creates the function 'getEig', which is used to calculate w and v analytically. 
getEig <- function(mat,transpose=F){
  
  vec <- Re(eigen(mat)$vectors[,1])                        #This calculates the right eigenvector 
  if(transpose==T) vec <- Re(eigen(t(mat))$vectors[,1])   #This calculates the left eigenvector 
  
  vec <- vec/(2*range(vec)[2]);                          #This standardizes the values by the largest value
  return(vec)
}



#Calculate sensititivites of yearly metalambda to seasonal vital rates and movement 
#Note that these sensitivity values are meaningless. Since they are complex combinations of demography and movement.
#They are only needed to calculate the seasonal sensitivities below

vw <-     array(0,c(2,4,length(y)*nRiver))
KMSens <- array(0,c(4,length(y)*nRiver,length(y)*nRiver))
KMElas <- array(0,c(4,length(y)*nRiver,length(y)*nRiver))
vDotW <-  array(0,c(4,length(y)*nRiver))

for (season in 1:4){
  f <- c(season, season : (season + 2) %% 4 + 1)
  
  vw[1,season,] <- getEig(metaKMyear[season,,],F)  #Must use getEigsens, not getEig. getEig are rescaled 
  vw[2,season,] <- getEig(metaKMyear[season,,],T)  
  
  KMSens[season,,] <- outer(vw[2,season,],vw[1,season,])
  vDotW[season,] <-     sum(vw[2,season,]*vw[1,season,])
  KMSens[season,,] <- KMSens[season,,]/vDotW[season,]
  KMElas[season,,] <- (valuesmetaKMyear[season]^(-1)) * (metaKMyear[season,,]) * KMSens[season,,]
 
}




#The following code calculates the sensitivity of lambda to either season migration or demography. Not both together.
#Calculate sensitivities of yearly metalambda to seasonal vital rates
KSensSeason <- array(0,c(4,length(y)*nRiver,length(y)*nRiver))
KElasSeason <- array(0,c(4,length(y)*nRiver,length(y)*nRiver))
for (season in 1:4){
  f <- c(season, season : (season + 2) %% 4 + 1)
  KSensSeason[season,,] <-t(t(perm)%*%metaM[f[4],,]%*%perm%*%metaK[f[4],,]%*%
    t(perm)%*%metaM[f[3],,]%*%perm%*%metaK[f[3],,]%*%
    t(perm)%*%metaM[f[2],,]%*%perm%*%metaK[f[2],,]%*%
    t(perm)%*%metaM[f[1],,]%*%perm)%*%
    KMSens[f[1],,]
  
  KElasSeason[season,,] <- (valuesmetaKMyear[season]^(-1)) * (metaK[season,,]) * KSensSeason[season,,]
  
  KSensSeason[season,,] <- KSensSeason[season,,] * diag(1,nRiver,nRiver) %x% array(1,c(length(y),length(y)))
  KElasSeason[season,,] <- KElasSeason[season,,] * diag(1,nRiver,nRiver) %x% array(1,c(length(y),length(y)))
 
  }

sum(KElasSeason) #Should equal nRiver



#Calculate sensitivities of yearly metalambda to seasonal movement
MSensSeason <- array(0,c(4,length(y)*nRiver,length(y)*nRiver))
MElasSeason <- array(0,c(4,length(y)*nRiver,length(y)*nRiver))
for (season in 1:4){
  f <- c(season, season : (season + 2) %% 4 + 1)
  MSensSeason[season,,] <-t(t(perm) %*% metaM[f[4],,] %*% perm %*% metaK[f[4],,] %*% 
    t(perm) %*% metaM[f[3],,] %*% perm %*% metaK[f[3],,] %*% 
    t(perm) %*% metaM[f[2],,] %*% perm %*% metaK[f[2],,] %*% 
    t(perm))%*% 
    KMSens[f[1],,] %*% 
    t(perm %*% metaK[f[1],,])     
  
  MElasSeason[season,,] <- (valuesmetaKMyear[season]^(-1)) * (metaM[season,,]) * MSensSeason[season,,]
  
  MSensSeason[season,,] <- MSensSeason[season,,] * diag(1,length(y)) %x% array(1,c(nRiver,nRiver))
  MElasSeason[season,,] <- MElasSeason[season,,] * diag(1,length(y)) %x% array(1,c(nRiver,nRiver))
  }

sum(MElasSeason) #Should equal nRiver


#Note that the above sensitivity and elasticity to movement are not really what we want since changing the prob of dispersing 
#to one river should be linked to a changed probability somehwere else. The way to account for this is to treat each 
#prob as a lower level parameter. This is below.
#pg19 Hunter and Caswell. For our case, it is in my (Ron) notebook pgs19-20.


#Note this will have to change if we do not allow individuals to move into river 4.
MSensSeason1 <- array(0,c(4,length(y)*nRiver,length(y)*nRiver))

for (season in 1:4){
  sumCol <- -colSums(MSensSeason[season,,]) ##The negative in front of colSums is suppose to be there
  for (mRow in 1:(length(y)*nRiver)){
    MSensSeason1[season,mRow,] <- sumCol + 2*MSensSeason[season,mRow,]  
  }
  rm(sumCol)
  MSensSeason[season,,] <- MSensSeason1[season,,]*diag(1,length(y)) %x% array(1,c(nRiver,nRiver))
  MElasSeason[season,,] <- (valuesmetaKMyear[season]^(-1)) * (metaM[season,,]) * MSensSeason[season,,] 
  }

sum(MElasSeason)    #Note that these will not sum to 1 or 4, because they include the covariance between migration probabilites#Caswellp232 
