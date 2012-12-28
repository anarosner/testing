
# Gxyshrink <- array(NA,c(length(y),4,nRiver))
# for (river in 1:nRiver){
#   for (season in 1:4){
#     for (size in 1:(length(y))){
#       Gxyshrink[size,season,river] <- sum(Gxy[(1:(size-1)),size,season,river])
#       Gxyshrink[1,season,river] <- 0
#     }
#   }
# }
# 
# Gxygrow <- array(0,c(length(y),length(y),4,nRiver))
# for (river in 1:nRiver){
#   for (season in 1:4){
#     for (size in 1:(length(y))){
#       Gxygrow[(size:length(y)),size,season,river] <- Gxy[(size:length(y)),size,season,river]
#       
#     }
#   }
# }
# 
# 
# Gxyb <- array(NA,c(length(y),length(y),4,nRiver))
# for (river in 1:nRiver){
#   for (season in 1:4){
#     Gxyb[,,season,river] <-  (diag(length(y))*Gxyshrink[,season,river])+Gxygrow[,,season,river]
#   }
# }
# 
# 
# 
# 
# for (river in 1:nRiver){
#   for (season in 1:4){
#     for (row in length(y)){
#        Gxy[row,,season,river] <- (1-colSums(Gxyb[,,season,river]))+Gxy[row,,season,river]
#     }
#   }
# }




######## Create demographic Kernels for each stream and season. 
Z <- array(NA,c(length(y),length(y),4,nRiver))
for (river in 1:nRiver) {
  for (season in 1:4){
    Z[,,season,river] <- Gxy[,,season,river]*Sx[,,season,river] 
  }
}

FEC <- array(NA,c(length(eggSize),length(y),4,nRiver))
for (river in 1:nRiver) {
    FEC[,,1,river] <- 0
    FEC[,,2,river] <- 0
    FEC[,,3,river] <- F1x[,,3,river]*F3x[,,3,river]*SJx[,,3,river]^2
    FEC[,,4,river] <- 0
    
}

RECRUIT <-  array(NA,c(n.big.matrix,eggSize,4,nRiver))
for (river in 1:nRiver) {
  RECRUIT[,,1,river] <- 0
  RECRUIT[,,2,river] <- F2xy[,,2,river]*SJx[,,2,river]^4
  RECRUIT[,,3,river] <- 0
  RECRUIT[,,4,river] <- 0
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

#Creates the kernel
K <- array(NA,c(length(y)+1,length(y)+1,4,nRiver))
for (season in 1:4){
  for (river in 1:nRiver){
    if (season == 1) {
    K[,,season,river] <- abind((abind(SJx[1,1,season,river]^2,RECRUIT[,,season,river],along=1)),(abind(FEC[,,season,river],Z[,,season,river],along=1)),along=2)  
    }
    if (season == 2){
    K[,,season,river] <- abind((abind(0,RECRUIT[,,season,river],along=1)),(abind(FEC[,,season,river],Z[,,season,river],along=1)),along=2)
    }  
    if (season == 3){
    K[,,season,river] <- abind((abind(0,RECRUIT[,,season,river],along=1)),(abind(FEC[,,season,river],Z[,,season,river],along=1)),along=2)
    }
    if (season == 4) {
    K[,,season,river] <- abind((abind(SJx[1,1,season,river]^4,RECRUIT[,,season,river],along=1)),(abind(FEC[,,season,river],Z[,,season,river],along=1)),along=2)  
    }
    }
}


#This creates the metaK, which is a block diagonal with rivers in each block on the diagonal. 
metaKrow4 <- metaKrow3 <-metaKrow2 <-metaKrow1 <- array(0,c(4,(length(y)+1),(length(y)+1)*nRiver))
metaK <- array(0,c(4,(length(y)+1)*nRiver,(length(y)+1)*nRiver))  #This is the whole metaK for a season 


for (season in 1:4){
  for (river in 1:nRiver){
    metaK[season,((river*(length(y)+1)-length(y)):(river*(length(y)+1))),((river*(length(y)+1)-length(y)):(river*(length(y)+1)))] <- K[,,season,river]
  }
}



##MetaM is the block diagonal matrix with migration. Movement probabilities for Stages in each block on the diagonal. 
#Within each block is the probabilities for stage i to move from river k to l 
metaMA <- array(0,c(4,(length(y))*nRiver,(length(y))*nRiver)) 

riverAreaM <- array(0,c(4,(length(y)+1)*nRiver,(length(y)+1)*nRiver))
for (season in 1:4){
  if (pop==1){
    metaMA[ season,, ] <- diag(1,length(y)) %x% psiA[ season,, ]   #this will have to change if psi is size-specific
  }

  if (pop>1){
    metaMA[ season,, ] <- diag(1,length(y)) %x% psiA[season]  #this will have to change if psi is size-specific
  }
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
    if (pop==1){
    metaKMyear[season,,] <-  riverArea[season,,]%*%t(perm) %*% metaM[f[4],,]  %*% perm %*% metaK[f[4],,] %*%
      t(perm) %*% metaM[f[3],,] %*% perm %*% metaK[f[3],,] %*%
      t(perm) %*% metaM[f[2],,] %*% perm %*% metaK[f[2],,] %*%
      t(perm) %*% metaM[f[1],,] %*% perm %*% metaK[f[1],,]%*%solve(riverArea[season,,])
    }
    if (pop>1){
      metaKMyear[season,,] <-  t(perm) %*% metaM[f[4],,] %*% perm %*% metaK[f[4],,] %*%
        t(perm) %*% metaM[f[3],,] %*% perm %*% metaK[f[3],,] %*%
        t(perm) %*% metaM[f[2],,] %*% perm %*% metaK[f[2],,] %*%
        t(perm) %*% metaM[f[1],,] %*% perm %*% metaK[f[1],,]
    }
    
  }









































if (stochastic==T){
  #Creates the kernel
  KRecruit <- array(NA,c(length(y)+1,length(y)+1,4,nRiver))
  for (season in 1:4){
    for (river in 1:nRiver){
      if (season == 1) {
        KRecruit[,,season,river] <- abind((abind(SJx[1,1,season,river]^2,RECRUIT[,,season,river],along=1)),(abind(FEC[,,season,river],array(0,c(length(y),length(y))),along=1)),along=2)  
      }
      if (season == 2){
        KRecruit[,,season,river] <- abind((abind(0,RECRUIT[,,season,river],along=1)),(abind(FEC[,,season,river],array(0,c(length(y),length(y))),along=1)),along=2)
      }  
      if (season == 3){
        KRecruit[,,season,river] <- abind((abind(0,RECRUIT[,,season,river],along=1)),(abind(FEC[,,season,river],array(0,c(length(y),length(y))),along=1)),along=2)
      }
      if (season == 4) {
        KRecruit[,,season,river] <- abind((abind(SJx[1,1,season,river]^4,RECRUIT[,,season,river],along=1)),(abind(FEC[,,season,river],array(0,c(length(y),length(y))),along=1)),along=2)  
      }
    }
  }
  KZ <- array(NA,c(length(y)+1,length(y)+1,4,nRiver))
  for (season in 1:4){
    for (river in 1:nRiver){
      if (season == 1) {
        KZ[,,season,river] <- abind((abind(0,array(0,c(length(y))),along=1)),(abind(array(0,c(length(y))),Z[,,season,river],along=1)),along=2)  
      }
      if (season == 2){
        KZ[,,season,river] <- abind((abind(0,array(0,c(length(y))),along=1)),(abind(array(0,c(length(y))),Z[,,season,river],along=1)),along=2)
      }  
      if (season == 3){
        KZ[,,season,river] <- abind((abind(0,array(0,c(length(y))),along=1)),(abind(array(0,c(length(y))),Z[,,season,river],along=1)),along=2)
      }
      if (season == 4) {
        KZ[,,season,river] <- abind((abind(0,array(0,c(length(y))),along=1)),(abind(array(0,c(length(y))),Z[,,season,river],along=1)),along=2)  
      }
    }
  }
  
  
  #This creates the metaK, which is a block diagonal with rivers in each block on the diagonal. 
  metaKrow4Recr <- metaKrow3Recr <-metaKrow2Recr <-metaKrow1Recr <- array(0,c(4,(length(y)+1),(length(y)+1)*nRiver))
  metaKrow4Z <- metaKrow3Z <-metaKrow2Z <-metaKrow1Z <- array(0,c(4,(length(y)+1),(length(y)+1)*nRiver))
  metaKRecr <- array(0,c(4,(length(y)+1)*nRiver,(length(y)+1)*nRiver))  #This is the whole metaK for a season 
  metaKZ <- array(0,c(4,(length(y)+1)*nRiver,(length(y)+1)*nRiver))  #This is the whole metaK for a season
  
  for (season in 1:4){
    for (river in 1:nRiver){
      metaKRecr[season,((river*(length(y)+1)-length(y)):(river*(length(y)+1))),((river*(length(y)+1)-length(y)):(river*(length(y)+1)))] <- KRecruit[,,season,river]
    }
  }
  for (season in 1:4){
    for (river in 1:nRiver){
      metaKZ[season,((river*(length(y)+1)-length(y)):(river*(length(y)+1))),((river*(length(y)+1)-length(y)):(river*(length(y)+1)))] <- KZ[,,season,river]
    }
  }
  

  
  ##loop through different starting seasons to get yearly KMyear matrices with different starting seasons
  metaKMyearRec <- array(0,c(4,(length(y)+1)*nRiver,(length(y)+1)*nRiver))
  metaKMyearZ <- array(0,c(4,(length(y)+1)*nRiver,(length(y)+1)*nRiver))
  
  
  for (season in 1:4){                         # season is the season in question and is the last season in the periodic matrix
    f <- c( season, season : (season+2) %% 4 + 1 )
    if (pop==1){
      metaKMyearRec[season,,] <-  riverArea[season,,]%*%t(perm) %*% metaM[f[4],,]  %*% perm %*% metaKRecr[f[4],,] %*%
        t(perm) %*% metaM[f[3],,] %*% perm %*% metaKRecr[f[3],,] %*%
        t(perm) %*% metaM[f[2],,] %*% perm %*% metaKRecr[f[2],,] %*%
        t(perm) %*% metaM[f[1],,] %*% perm %*% metaKRecr[f[1],,]%*%solve(riverArea[season,,])
    }
    if (pop>1){
      metaKMyearRec[season,,] <-  t(perm) %*% metaM[f[4],,] %*% perm %*% metaKRecr[f[4],,] %*%
        t(perm) %*% metaM[f[3],,] %*% perm %*% metaKRecr[f[3],,] %*%
        t(perm) %*% metaM[f[2],,] %*% perm %*% metaKRecr[f[2],,] %*%
        t(perm) %*% metaM[f[1],,] %*% perm %*% metaKRecr[f[1],,]
    }
    
  }
  
  for (season in 1:4){                         # season is the season in question and is the last season in the periodic matrix
    f <- c( season, season : (season+2) %% 4 + 1 )
    if (pop==1){
      metaKMyearZ[season,,] <-  riverArea[season,,]%*%t(perm) %*% metaM[f[4],,]  %*% perm %*% metaKZ[f[4],,] %*%
        t(perm) %*% metaM[f[3],,] %*% perm %*% metaKZ[f[3],,] %*%
        t(perm) %*% metaM[f[2],,] %*% perm %*% metaKZ[f[2],,] %*%
        t(perm) %*% metaM[f[1],,] %*% perm %*% metaKZ[f[1],,]%*%solve(riverArea[season,,])
    }
    if (pop>1){
      metaKMyearZ[season,,] <-  t(perm) %*% metaM[f[4],,] %*% perm %*% metaKZ[f[4],,] %*%
        t(perm) %*% metaM[f[3],,] %*% perm %*% metaKZ[f[3],,] %*%
        t(perm) %*% metaM[f[2],,] %*% perm %*% metaKZ[f[2],,] %*%
        t(perm) %*% metaM[f[1],,] %*% perm %*% metaKZ[f[1],,]
    }
    
  }
  
}

