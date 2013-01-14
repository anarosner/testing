

if (splitStreams==F){
  nRiver <- nOpenLargeMain + nOpenLarge + nOpenSmall +nIsolatedSmall
}
if (splitStreams==T){
  if (pop==1){
    nRiver <- nOpenLargeMain + nOpenLarge + nOpenSmall 
       
  }
  if (pop>1){
    nRiver <- nIsolatedSmall
  }
}





if (nOpenLargeMain>0){
  OLM <- data.frame(1:nOpenLargeMain,1)
  OLM[,1] <- 1 
}
if (nOpenLargeMain==0){
  OLM <- data.frame(nOpenLargeMain,1)
}
if (nOpenLarge>0){
  OL <- data.frame(1:nOpenLarge,2)
  OL[,1] <- 1
}
if (nOpenLarge==0){
  OL <- data.frame(nOpenLarge,2)
}
if (nOpenSmall>0){
  OS <- data.frame(1:nOpenSmall,3)
  OS[,1] <- 1
}
if (nOpenSmall==0){
  OS <- data.frame(nOpenSmall,3)
}
if (nIsolatedSmall>0){
  ISO <- data.frame(1:nIsolatedSmall,4)
  ISO[,1] <- 1
}
if (nIsolatedSmall==0){
  ISO <- data.frame(nIsolatedSmall,4)
}
colnames(OLM) <- c("Stream Number","Stream Type")
colnames(OL) <- c("Stream Number","Stream Type")
colnames(OS) <- c("Stream Number","Stream Type")
colnames(ISO) <- c("Stream Number","Stream Type")



if(splitStreams==F){
metaRiverA <- rbind(OLM,OL,OS,ISO,deparse.level = 1)
if(dim(metaRiverA)[1]!=nRiver){
  metaRiverA <- metaRiverA[-which(metaRiverA[,1]==0),] 
  }

metaRiverA[,1] <- c(1:nRiver)
}


if (splitStreams==T){
  if (pop==1){
    metaRiverA <- rbind(OLM,OL,OS,deparse.level = 1)
    if(dim(metaRiverA)[1]!=nRiver){
      
      metaRiverA <- metaRiverA[-which(metaRiverA[,1]==0),] 
    }
       
    metaRiverA[,1] <- c(1:nRiver)
  }
  
  if (pop>1){
    metaRiverA <- rbind(ISO,deparse.level = 1)     #If more isolated are added then this must look like the main pop one
      
    metaRiverA[,1] <- c(1:nRiver)
  }
}


if (pop==1){
  metaRiver <- metaRiverA
}
if (pop>1){
  metaRiver <- metaRiverA[(pop-1),]
  nRiver <- 1
}
