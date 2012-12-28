


######## Create demographic Kernels for each stream and season. 
Z <- array(NA,c(length(y),length(y),4,nRiver))
#Construct the kernel 
for (river in 1:nRiver) {
  for (season in 1:4){
    Z[,,season,river] <- Gxy[,,season,river]*Sx[,,season,river] 
  }
}

FEC <- array(NA,c(length(eggSize),length(y),4,nRiver))
#Construct the kernel 
for (river in 1:nRiver) {
  for (season in 1:4){
    FEC[,,season,river] <- F1x[,,season,river]*F3x[,,season,river]*SJx[,,season,river]^.5
 
  }
}

RECRUIT <-  array(NA,c(n.big.matrix,eggSize,4,nRiver))
for (river in 1:nRiver) {
  for (season in 1:4){
    RECRUIT[,,season,river] <- F2xy[,,season,river]*SJx[,,season,river]
  }
}


#This creates the vec-permutation matrix 
perm <- array(0,c((length(y)+1)*nRiver,(length(y)+1)*nRiver))
for (s in 1:length(y)){
  for (p in 1:nRiver){
    E <- array(0,c(length(y)+1,nRiver))
    E[s,p] <- 1 
    Et <- t(E)
    Ek <- E %x% Et
    perm  <- perm + Ek
  }
}

K <- array(NA,c(length(y)+1,length(y)+1,4,nRiver))
for (season in 1:4){
  for (river in 1:nRiver){
    if (season == 1) {
    K[,,season,river] <- abind((abind(SJx[1,1,season,river],RECRUIT[,,season,river],along=1)),(abind(FEC[,,season,river],Z[,,season,river],along=1)),along=2)  
    }
    if (season == 2){
    K[,,season,river] <- abind((abind(0,RECRUIT[,,season,river],along=1)),(abind(FEC[,,season,river],Z[,,season,river],along=1)),along=2)
    }  
    if (season == 3){
    K[,,season,river] <- abind((abind(0,RECRUIT[,,season,river],along=1)),(abind(FEC[,,season,river],Z[,,season,river],along=1)),along=2)
    }
    if (season == 4) {
    K[,,season,river] <- abind((abind(SJx[1,1,season,river],RECRUIT[,,season,river],along=1)),(abind(FEC[,,season,river],Z[,,season,river],along=1)),along=2)  
    }
    }
}


#This creates the metaK, which is a block diagonal with rivers in each block on the diagonal. 
#There should be a more versatile way to do this.If there are more rivers. But this is what I have for now

metaKrow4 <- metaKrow3 <-metaKrow2 <-metaKrow1 <- array(0,c(4,(length(y)+1),(length(y)+1)*nRiver))
metaK <- array(0,c(4,(length(y)+1)*nRiver,(length(y)+1)*nRiver))  #This is the whole metaK for a season 


zeros <- array(0,c((length(y)+1),(length(y)+1)))

for (season in 1:4){
  metaKrow1[season,,] <- abind(K[,,season,1],zeros,zeros,zeros, along=2)
  metaKrow2[season,,] <- abind(zeros,K[,,season,2],zeros,zeros, along=2)
  metaKrow3[season,,] <- abind(zeros,zeros,K[,,season,3],zeros, along=2)
  metaKrow4[season,,] <- abind(zeros,zeros,zeros,K[,,season,4], along=2)
  
  metaK[season,,] <- abind(metaKrow1[season,,],metaKrow2[season,,],metaKrow3[season,,],metaKrow4[season,,],along=1)
  
}

##MetaM is the block diagonal matrix with migration. Movement probabilities for Stages in each block on the diagonal. 
#Within each block is the probabilities for stage i to move from river k to l 
metaMA <- array(0,c(4,(length(y))*nRiver,(length(y))*nRiver)) 
for (season in 1:4){
  metaMA[ season,, ] <- diag(1,length(y)) %x% psiA[ season,, ]   #this will have to change if psi is size-specific
}


metaM <- array(0,c(4,(length(y)+1)*nRiver,(length(y)+1)*nRiver)) 
metaMb <- array(0,c(4,(length(y)+1)*nRiver,length(y)*nRiver))
metaMc <- array(0,c(4,(length(y)+1)*nRiver,nRiver))
zeros <- array(0,c(nRiver,length(y)*nRiver))
for (season in 1:4){
  metaMb[season,,] <- abind(zeros,metaMA[season,,],along=1) 
  metaMc[season,,] <- abind(psiE,t(zeros),along=1)
  metaM[season,,] <- abind(metaMc[season,,],metaMb[season,,],along=2)
}



##loop through different starting seasons to get yearly KMyear matrices with different starting seasons
metaKMyear <- array(0,c(4,(length(y)+1)*nRiver,(length(y)+1)*nRiver))

for (season in 1:4){                         # season is the season in question and is the last season in the periodic matrix
  f <- c( season, season : (season+2) %% 4 + 1 )
  metaKMyear[season,,] <-  t(perm) %*% metaM[f[4],,] %*% perm %*% metaK[f[4],,] %*%
    t(perm) %*% metaM[f[3],,] %*% perm %*% metaK[f[3],,] %*%
    t(perm) %*% metaM[f[2],,] %*% perm %*% metaK[f[2],,] %*%
    t(perm) %*% metaM[f[1],,] %*% perm %*% metaK[f[1],,]
  
}




