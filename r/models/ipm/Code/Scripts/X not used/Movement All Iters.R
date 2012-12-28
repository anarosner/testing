#Movement for Eggs. Sounds stupid, but needs to be there.
psiE <- diag(1,nRiver,nRiver)


#Movement for Fish
if (move==T){
psiA <- array( NA,c(4,4,4) )
for ( season in 1:4 ){
  
   psiA[season,,] <- t( pList$psiBeta[season,,,iter,1] )
  
  }
}


if (move==F) {
  psiA <- array( NA,c(4,nRiver,nRiver) )
  for ( season in 1:4 ){
    psiA[season,,] <- diag(nRiver)
  }  
}




##This is to eliminate the probability of fish moving into Obear
if (ObearOff==T){
  if (splitStreams==F){
    for (season in 1:4){
      psiA[season,4,1:3] <- 0
      psiA[season,,] <- (psiA[season,,]+t(((1-colSums(psiA[season,,]))/3) * array(1,c(4,4))))
      psiA[season,4,1:3] <- 0
    }
  }
}

if (splitStreams==T){
  for (season in 1:4){
    psiA[season,4,1:3] <- 0
    psiA[season,,] <- (psiA[season,,]+t(((1-colSums(psiA[season,,]))/3) * array(1,c(4,4))))
    psiA[season,4,1:3] <- 0
      
    }
  if (pop==1){
    psiA <- psiA[,1:3,1:3]
    metaPsi <- array(0,c(4,nRiver,nRiver))
    
    for (season in 1:4){
      for (riverTo in 1:nRiver){
        for (riverFrom in 1:nRiver){
          type <- metaRiver[riverFrom,2]
          totype  <- metaRiver[riverTo,2]
          if (totype!=type){
            metaPsi[season,riverTo,riverFrom] <- psiA[season,totype,type] 
          }
          if (totype==type){
            if (riverTo!=riverFrom){
              metaPsi[season,riverTo,riverFrom] <- psiA[season,totype,1]*psiA[season,1,type] 
            }
            if (riverTo==riverFrom){
              if (nRiver>1){
                metaPsi[season,,] <-  metaPsi[season,,] + diag(1,nRiver)*(1-colSums(metaPsi[season,,]))
              }
              if (nRiver==1){
                metaPsi[season,,] <-  metaPsi[season,,] + diag(1,nRiver)*(1-sum(metaPsi[season,,]))
              }
            }
            
          }
        
        }
      }
    }
    psiA <- metaPsi
  }  
  
  
  
  if (pop>1){
    psiA <- psiA[,4,4]

  }
  
}
  


