if (yr == 1){
  popNobsbyYearSeasonSizeAge <- array(NA,c((years+1),4,4,3,length(y)))
  
  for (ye in 1:(years+1)){
    for (season in 1:4){
      popNobsbyYearSeasonSizeAge[ye,season,1,2,] <- hist(dMData$length[which(dMData$season==season & dMData$year==(2002+ye) & dMData$river=='WEST BROOK' & dMData$age==0)],breaks=abind(L,y+h/2,along=1),plot=F)$counts
      popNobsbyYearSeasonSizeAge[ye,season,1,3,] <- hist(dMData$length[which(dMData$season==season & dMData$year==(2002+ye) & dMData$river=='WEST BROOK' & dMData$age!=0)],breaks=abind(L,y+h/2,along=1),plot=F)$counts
      
      popNobsbyYearSeasonSizeAge[ye,season,2,2,] <- hist(dMData$length[which(dMData$season==season & dMData$year==(2002+ye) & dMData$river=='WB JIMMY' & dMData$age==0)],breaks=abind(L,y+h/2,along=1),plot=F)$counts
      popNobsbyYearSeasonSizeAge[ye,season,2,3,] <- hist(dMData$length[which(dMData$season==season & dMData$year==(2002+ye) & dMData$river=='WB JIMMY' & dMData$age!=0)],breaks=abind(L,y+h/2,along=1),plot=F)$counts
      
      popNobsbyYearSeasonSizeAge[ye,season,3,2,] <- hist(dMData$length[which(dMData$season==season & dMData$year==(2002+ye) & dMData$river=='WB MITCHELL' & dMData$age==0)],breaks=abind(L,y+h/2,along=1),plot=F)$counts
      popNobsbyYearSeasonSizeAge[ye,season,3,3,] <- hist(dMData$length[which(dMData$season==season & dMData$year==(2002+ye) & dMData$river=='WB MITCHELL' & dMData$age!=0)],breaks=abind(L,y+h/2,along=1),plot=F)$counts
      
      popNobsbyYearSeasonSizeAge[ye,season,4,2,] <- hist(dMData$length[which(dMData$season==season & dMData$year==(2002+ye) & dMData$river=='WB OBEAR' & dMData$age==0)],breaks=abind(L,y+h/2,along=1),plot=F)$counts
      popNobsbyYearSeasonSizeAge[ye,season,4,3,] <- hist(dMData$length[which(dMData$season==season & dMData$year==(2002+ye) & dMData$river=='WB OBEAR' & dMData$age!=0)],breaks=abind(L,y+h/2,along=1),plot=F)$counts
      
      popNobsbyYearSeasonSizeAge[ye,season,1,1,] <- hist(dMData$length[which(dMData$season==season & dMData$year==(2002+ye) & dMData$river=='WEST BROOK')],breaks=abind(L,y+h/2,along=1),plot=F)$counts
      popNobsbyYearSeasonSizeAge[ye,season,2,1,] <- hist(dMData$length[which(dMData$season==season & dMData$year==(2002+ye) & dMData$river=='WB JIMMY')],breaks=abind(L,y+h/2,along=1),plot=F)$counts
      popNobsbyYearSeasonSizeAge[ye,season,3,1,] <- hist(dMData$length[which(dMData$season==season & dMData$year==(2002+ye) & dMData$river=='WB MITCHELL')],breaks=abind(L,y+h/2,along=1),plot=F)$counts
      popNobsbyYearSeasonSizeAge[ye,season,4,1,] <- hist(dMData$length[which(dMData$season==season & dMData$year==(2002+ye) & dMData$river=='WB OBEAR')],breaks=abind(L,y+h/2,along=1),plot=F)$counts
      
    }
  }
  
  probCatch <- (out$pBeta[,2:10,2:5,,1])

if (EnvVarsFromObsWestBrook==T) {
  popNobsbyYearSeasonSizeAgeAdj <- array(NA,c((years+1),4,4,3,length(y)))
  for (ye in 1:(years+1)){
    for (r in 1:4){
      for (season in 1:4){
        for (age in 1:3){
          popNobsbyYearSeasonSizeAgeAdj[ye,season,r,age,] <-  (popNobsbyYearSeasonSizeAge[ye,season,r,age,]/mean(invlogit(probCatch[season,ye,r,])))/2 #div by two to get females
          
        }
      }
    }
  }
  
  popNobsbyYearSeasonSizeAgeAdjSum <- array(NA,c((years+1),4,4,3))
  for (ye in 1:(years+1)){
    for (r in 1:4){
      for (season in 1:4){
        for (age in 1:3){
          popNobsbyYearSeasonSizeAgeAdjSum[ye,season,r,age] <-  sum(popNobsbyYearSeasonSizeAgeAdj[ye,season,r,age,])
        }
      }
    }
  }
} 
  
if (EnvVarsFromWeatherGen==T) {
  popNobsbyYearSeasonSizeAgeAdj <- array(NA,c((years+1),4,4,3,length(y)))

    for (r in 1:4){
      for (season in 1:4){
        for (age in 1:3){
          popNobsbyYearSeasonSizeAgeAdj[1,season,r,age,] <-  (popNobsbyYearSeasonSizeAge[1,season,r,age,]/mean(invlogit(probCatch[season,1,r,])))/2 #div by two to get females
          
  
      }
    }
  }
  
  popNobsbyYearSeasonSizeAgeAdjSum <- array(NA,c((years+1),4,4,3))

    for (r in 1:4){
      for (season in 1:4){
        for (age in 1:3){
          popNobsbyYearSeasonSizeAgeAdjSum[1,season,r,age] <-  sum(popNobsbyYearSeasonSizeAgeAdj[1,season,r,age,])
   
      }
    }
  }
}


  ylong <- array(0,c(length(y)+1,nRiver))
  for(river in 1:nRiver){
    ylong[2:(length(y)+1),river] <- y
  }
  ylong <- c(as.matrix(ylong))
  
  if (pop==1){
    if(initialSizeObs==F){    
      initialN <- 754*brookAnalysis$vectorsMain[1,3,]/2
    }
    if(initialSizeObs==T){
       initialN <-abind(abind(0,popNobsbyYearSeasonSizeAgeAdj[1,3,1,1,],along=1),
                     abind(0,popNobsbyYearSeasonSizeAgeAdj[1,3,2,1,],along=1),abind(0,popNobsbyYearSeasonSizeAgeAdj[1,3,3,1,],along=1),along=1)
    }
  }
  
  if (pop>1){
    if(initialSizeObs==F){
        initialN <- 127*brookAnalysis$vectorsIso[1,3,]/2
    }
    if(initialSizeObs==T){
        initialN <-abind(0,popNobsbyYearSeasonSizeAgeAdj[1,3,4,1,],along=1)
    }
  }
  
  
  if (means==F){
    Nt <- initialN
    
    if (pop==1){
      popSize[pop,iter,1,1] <- sum(initialN)
      popSize[pop,iter,1,2] <- sum(popNobsbyYearSeasonSizeAgeAdjSum[1,3,1:3,2])
      popSize[pop,iter,1,3] <- sum(popNobsbyYearSeasonSizeAgeAdjSum[1,3,1:3,3])
      
      NDistMain[iter,1,1,] <- initialN
      NDistMain[iter,1,2,] <- abind(0,(popNobsbyYearSeasonSizeAgeAdj[1,3,1,2,]),0,(popNobsbyYearSeasonSizeAgeAdj[1,3,2,2,]),0,(popNobsbyYearSeasonSizeAgeAdj[1,3,3,2,]),along=1)
      NDistMain[iter,1,3,] <- abind(0,(popNobsbyYearSeasonSizeAgeAdj[1,3,1,3,]),0,(popNobsbyYearSeasonSizeAgeAdj[1,3,2,3,]),0,(popNobsbyYearSeasonSizeAgeAdj[1,3,3,3,]),along=1)
    }
    if (pop>1){
      popSize[pop,iter,1,1] <- sum(initialN)
      popSize[pop,iter,1,2] <- sum(popNobsbyYearSeasonSizeAgeAdjSum[1,3,4,2])
      popSize[pop,iter,1,3] <- sum(popNobsbyYearSeasonSizeAgeAdjSum[1,3,4,3])
      NDistIso[pop-1,iter,1,1,] <- initialN
      NDistIso[pop-1,iter,1,2,] <- abind(0,(popNobsbyYearSeasonSizeAgeAdj[1,3,4,2,]),along=1)
      NDistIso[pop-1,iter,1,3,] <- abind(0,(popNobsbyYearSeasonSizeAgeAdj[1,3,4,3,]),along=1)
    }
  }
  
  if (means==T){
    Nt <- initialN
      
    if (pop==1){
      popSize[pop,1,1] <- sum(initialN)
      popSize[pop,1,2] <- sum(popNobsbyYearSeasonSizeAgeAdjSum[1,3,1:3,2])
      popSize[pop,1,3] <- sum(popNobsbyYearSeasonSizeAgeAdjSum[1,3,1:3,3])
      NDistMain[1,1,] <- initialN
      NDistMain[1,2,] <- abind(0,(popNobsbyYearSeasonSizeAgeAdj[1,3,1,2,]),0,(popNobsbyYearSeasonSizeAgeAdj[1,3,2,2,]),0,(popNobsbyYearSeasonSizeAgeAdj[1,3,3,2,]),along=1)
      NDistMain[1,3,] <- abind(0,(popNobsbyYearSeasonSizeAgeAdj[1,3,1,3,]),0,(popNobsbyYearSeasonSizeAgeAdj[1,3,2,3,]),0,(popNobsbyYearSeasonSizeAgeAdj[1,3,3,3,]),along=1)
    }
    if (pop>1){
      popSize[pop,1,1] <- sum(initialN)
      popSize[pop,1,2] <- sum(popNobsbyYearSeasonSizeAgeAdjSum[1,3,4,2])
      popSize[pop,1,3] <- sum(popNobsbyYearSeasonSizeAgeAdjSum[1,3,4,3])
      NDistIso[pop-1,1,1,] <- initialN
      NDistIso[pop-1,1,2,] <- abind(0,(popNobsbyYearSeasonSizeAgeAdj[1,3,4,2,]),along=1)
      NDistIso[pop-1,1,3,] <- abind(0,(popNobsbyYearSeasonSizeAgeAdj[1,3,4,3,]),along=1)
    }
  }
  iteration=function(Nt) {
    Nt1 <- metaKMyear[3,,]%*%Nt
    
    return(Nt1)
  }
  
  iterationRec=function(Nt) {
    Nt1Rec <- metaKMyearRec[3,,]%*%Nt
    
    return(Nt1Rec)
  }
  iterationAd=function(Nt) {
    Nt1Ad <- metaKMyearZ[3,,]%*%Nt
    
    return(Nt1Ad)
  }
  
  
}

  

    for (yr in 1:years){
#      if (sum(Nt) > 10){
          tempert <- flowpert <- yr
       
          demoVariables <- array(0,c((nOpenLargeMain + nOpenLarge + nOpenSmall + nIsolatedSmall),4,2))
          dimnames(demoVariables) <- list(c(1:(nOpenLargeMain + nOpenLarge + nOpenSmall + nIsolatedSmall)),c("Fall","Winter","Spring","Summer"),c("Flow","Temp"))

          
#This is for the livestreams 
          if (EnvVarsFromWeatherGen==T){
            demoVariables[,,1] <- t(streamVars[((yr*4)-3):(4*yr),7:10])
            demoVariables[,,2] <- t(streamVars[((yr*4)-3):(4*yr),3:6])
          }
##This is for a total random environment          
          if (EnvVarsRandomDraw==T){
            for (season in 1:4){
             demoVariables[,season,1] <- rnorm(n=1,mean=0,sd=1)
             demoVariables[,season,2] <- rnorm(n=1,mean=0,sd=1)
              }
           }
                 
###This is for the observed variation
          if(EnvVarsFromObsWestBrook==T){
           demoVariables[,,1] <- t(MeansBySeasonRiverYear[((yr*4)-3):(4*yr),7:10])
           demoVariables[,,2] <- t(MeansBySeasonRiverYear[((yr*4)-3):(4*yr),3:6])     
          }
          
          source(paste0(code_dir,"/Scripts/ConstCompMatricesAllStreamsStoch.R"))
          source(paste0(code_dir,"/Scripts/MetaKM.R"))
          
          if (means==F){

            
            
            Nt1 = iteration(Nt)
            Nt1Rec = iterationRec(Nt)
            Nt1Ad = iterationAd(Nt)
            
            popSize[pop,iter,(yr+1),1] <- sum(Nt1)
            popSize[pop,iter,(yr+1),2] <- sum(Nt1Rec)
            popSize[pop,iter,(yr+1),3] <- sum(Nt1Ad)
            
            yearlyLamb[pop,iter,yr] <- (sum(Nt1)/sum(Nt))
            if (pop==1){
              NDistMain[iter,(yr+1),1,] <- Nt1
              NDistMain[iter,(yr+1),2,] <- Nt1Rec
              NDistMain[iter,(yr+1),3,] <- Nt1Ad
            }
            if (pop>1){
              NDistIso[(pop-1),iter,(yr+1),1,] <- Nt1
              NDistIso[(pop-1),iter,(yr+1),2,] <- Nt1Rec
              NDistIso[(pop-1),iter,(yr+1),3,] <- Nt1Ad
            }
            Nt <- Nt1
   
            stochLamb1[pop,iter,yr] <- (log(popSize[pop,iter,yr,1])-log(popSize[pop,iter,1,1]))/yr
            stochLamb2[pop,iter,yr] <- sum(log(yearlyLamb[pop,iter,]))/yr
                   
            print(c(pop,"-",iter,"-",yr))
          }
          
          if (means==T){
            Nt1 = iteration(Nt)
            Nt1Rec = iterationRec(Nt)
            Nt1Ad = iterationAd(Nt)
            
            popSize[pop,(yr+1),1] <- sum(Nt1)
            popSize[pop,(yr+1),2] <- sum(Nt1Rec)
            popSize[pop,(yr+1),3] <- sum(Nt1Ad)
            
            yearlyLamb[pop,yr] <- (sum(Nt1)/sum(Nt))
            if (pop==1){
              NDistMain[(yr+1),1,] <- Nt1
              NDistMain[(yr+1),2,] <- Nt1Rec
              NDistMain[(yr+1),3,] <- Nt1Ad
            }
            if (pop>1){
              NDistIso[(pop-1),(yr+1),1,] <- Nt1
              NDistIso[(pop-1),(yr+1),2,] <- Nt1Rec
              NDistIso[(pop-1),(yr+1),3,] <- Nt1Ad
            }
            Nt <- Nt1
            
            stochLamb1[pop,yr] <- (log(popSize[pop,yr,1])-log(popSize[pop,1,1]))/yr
            stochLamb2[pop,yr] <- sum(log(yearlyLamb[pop,]))/yr
            
            print(c(pop,"-",yr))
          }
          
    }
     
#}


if (means==F){
stochLambda <- array(0,c(npop,nIter))
  for (p in 1:2){
    stochLambda[p,] <- exp((stochLamb2[p,,years]))
  }  
dimnames(stochLambda) <- list(c("Westbrook","Obear"),c(1:nIter))
}

if (means==T){
  stochLambda <- array(0,c(npop))
  for (p in 1:2){
    stochLambda[p] <- exp(mean(stochLamb2[p,years]))
  }  
  dimnames(stochLambda) <- list(c("Westbrook","Obear"))
}


