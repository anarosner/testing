

#============================================================================================#
# (III) Create the function 'getEig', which is used to calculate w and v analytically
#       getEigsens should be used to calculate sensitivities
#============================================================================================#

#This creates the function 'getEig', which is used to calculate w and v analytically. NOte these are rescaled values.
#Should not be used to calculate sensitivities or elasticities
getEig <- function(mat,transpose=F){
  
  vec <- Re(eigen(mat)$vectors[,1])                        #This calculates the right eigenvector 
  if(transpose==T) vec <- Re(eigen(t(mat))$vectors[,1])   #This calculates the left eigenvector 
  
  vec <- vec/(2*range(vec)[2]);                          #This standardizes the values by the largest value
  return(vec)
}

getEigsens <- function(mat,transpose=F){                  #This one can be used to calculate sensitivities and elasticities
  
  vec <- Re(eigen(mat)$vectors[,1])                        #This calculates the right eigenvector 
  if(transpose==T) vec <- Re(eigen(t(mat))$vectors[,1])    #This calculates the left eigenvector 
  return(vec)
}


#Calculate sensititivites of yearly metalambda to seasonal vital rates and movement 
#Note that these values are meaningless. They are only needed to calculate the seasonal sensitivities below

vw <-     array(0,c(2,4,length(y)*nRiver))
KMSens <- array(0,c(4,length(y)*nRiver,length(y)*nRiver))
KMElas <- array(0,c(4,length(y)*nRiver,length(y)*nRiver))
vDotW <-  array(0,c(4,length(y)*nRiver))

for (firstSeason in 1:4){
  f <- c(firstSeason, firstSeason : (firstSeason + 2) %% 4 + 1)
  
  vw[1,firstSeason,] <- getEigsens(metaKMyear[firstSeason,,],F)  #Must use getEigsens, not getEig. getEig are rescaled 
  vw[2,firstSeason,] <- getEigsens(metaKMyear[firstSeason,,],T)  
  
  KMSens[firstSeason,,] <- outer(vw[2,firstSeason,],vw[1,firstSeason,])
  vDotW[firstSeason,] <-     sum(vw[2,firstSeason,]*vw[1,firstSeason,])
  KMSens[firstSeason,,] <- KMSens[firstSeason,,]/vDotW[firstSeason,]
  KMElas[firstSeason,,] <- (valuesmetaKMyear[firstSeason]^(-1)) * (metaKMyear[firstSeason,,]) * KMSens[firstSeason,,]
  
 
}




#The following code calculates the sensitivity of lambda to either season migration or vital rates. Not both together.
#Calculate sensitivities of yearly metalambda to seasonal vital rates
KSensSeason <- array(0,c(4,length(y)*nRiver,length(y)*nRiver))
KElasSeason <- array(0,c(4,length(y)*nRiver,length(y)*nRiver))
for (firstSeason in 1:4){
  f <- c(firstSeason, firstSeason : (firstSeason + 2) %% 4 + 1)
  KSensSeason[firstSeason,,] <-t(t(perm)%*%metaM[f[4],,]%*%perm%*%metaK[f[4],,]%*%
    t(perm)%*%metaM[f[3],,]%*%perm%*%metaK[f[3],,]%*%
    t(perm)%*%metaM[f[2],,]%*%perm%*%metaK[f[2],,]%*%
    t(perm)%*%metaM[f[1],,]%*%perm)%*%
    KMSens[f[1],,]
  
  KElasSeason[firstSeason,,] <- (valuesmetaKMyear[firstSeason]^(-1)) * (metaK[firstSeason,,]) * KSensSeason[firstSeason,,]
  
  KSensSeason[firstSeason,,] <- KSensSeason[firstSeason,,] * diag(1,nRiver,nRiver) %x% array(1,c(length(y),length(y)))
  KElasSeason[firstSeason,,] <- KElasSeason[firstSeason,,] * diag(1,nRiver,nRiver) %x% array(1,c(length(y),length(y)))
 
  }


sum(KElasSeason) #Should equal nRiver



#Calculate sensitivities of yearly metalambda to seasonal movement
MSensSeason <- array(0,c(4,length(y)*nRiver,length(y)*nRiver))
MElasSeason <- array(0,c(4,length(y)*nRiver,length(y)*nRiver))
for (firstSeason in 1:4){
  f <- c(firstSeason, firstSeason : (firstSeason + 2) %% 4 + 1)
  MSensSeason[firstSeason,,] <-t(t(perm) %*% metaM[f[4],,] %*% perm %*% metaK[f[4],,] %*% 
    t(perm) %*% metaM[f[3],,] %*% perm %*% metaK[f[3],,] %*% 
    t(perm) %*% metaM[f[2],,] %*% perm %*% metaK[f[2],,] %*% 
    t(perm))%*% 
    KMSens[f[1],,] %*% 
    t(perm %*% metaK[f[1],,])     
  
  MElasSeason[firstSeason,,] <- (valuesmetaKMyear[firstSeason]^(-1)) * (metaM[firstSeason,,]) * MSensSeason[firstSeason,,]
  
  MSensSeason[firstSeason,,] <- MSensSeason[firstSeason,,] * diag(1,length(y)) %x% array(1,c(nRiver,nRiver))
  MElasSeason[firstSeason,,] <- MElasSeason[firstSeason,,] * diag(1,length(y)) %x% array(1,c(nRiver,nRiver))
  
  
  }

sum(MElasSeason) #Should equal nRiver


#Note that the above sensitivity and elasticity to movement are not really what we want since changing the prob of dispersing 
#to one river should be linked to a changed probability somehwere else. The way to account for this is to treat each 
#prob as a lower level parameter. This is below.
#pg19 Hunter and Caswell. For our case, it is in my (Ron) notebook pgs19-20.

MSensSeason1 <- array(0,c(4,length(y)*nRiver,length(y)*nRiver))

for (firstSeason in 1:4){
  sumCol <- -colSums(MSensSeason[firstSeason,,]) ##The negative in front of colSums is suppose to be there
  for (mRow in 1:(length(y)*nRiver)){
    MSensSeason1[firstSeason,mRow,] <- sumCol + 2*MSensSeason[firstSeason,mRow,]  
  }
  rm(sumCol)
  MSensSeason[firstSeason,,] <- MSensSeason1[firstSeason,,]*diag(1,length(y)) %x% array(1,c(nRiver,nRiver))
  MElasSeason[firstSeason,,] <- (valuesmetaKMyear[firstSeason]^(-1)) * (metaM[firstSeason,,]) * MSensSeason[firstSeason,,] 
  
 
  }

sum(MElasSeason)    #Note that these will not sum to 1 or 4, because they include the covariance between migration probabilites#Caswellp232 
