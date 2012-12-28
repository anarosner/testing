
if (splitStreams==T){
  npop <- 1+nIsolatedSmall
}

if (splitStreams==F){
  npop <- 1
}


source("Code/Scripts/CreateResultsMatricesAllStreams.R")




for (pop in 1:npop){
  
  source("Code/Scripts/RiverDims.R")
  source("Code/Scripts/Movement All Iters.R")                #Creates the movement matrices. Right not is not function of flow or temp, so is outside loops
  source("Code/Scripts/Demographic Functions All Iters.R")


  for (firstSeason in 1:nSeason){
    for (temppert in 1:(length(tvariables))){
      for (flowpert in c(which(fvariables==-1),which(fvariables==0),which(fvariables==1))){
          
        if (zeroCond==F){
          #This inputs the standardized variable values for each stream and season for flow and temp. Can be inputed directly from web site   
          demoVariables <- array(0,c((nOpenLargeMain + nOpenLarge + nOpenSmall + nIsolatedSmall),4,2))
          dimnames(demoVariables) <- list(c(1:(nOpenLargeMain + nOpenLarge + nOpenSmall + nIsolatedSmall)),c("Spring","Summer","Fall","Winter"),c("Flow","Temp"))
          #For now, can be changed using this code. BY default, all values are now zero, which is the mean.
          #demoVariables[3,1,2] <- 10
          demoVariables[,firstSeason,1] <- fvariables[flowpert]
          demoVariables[,firstSeason,2] <- tvariables[temppert]
        }
  
          source("Code/Scripts/ConstCompMatricesAllStreams.R")
          source("Code/Scripts/MetaKM.R")
          if (Analytical==F){
            source("Code/Scripts/EigCalcNumerical.R")
          }
          if (Analytical==T){
            source("Code/Scripts/EigCalcAnalytical.R") 
          }   
          
          if (zeroCond==T){
            source("Code/Scripts/SensElas.R")
            source("Code/Scripts/SensVitalRates.R")
          }
          
          #yearly lambda
          if (zeroCond==F){
            metaLambdas[pop,firstSeason,flowpert,temppert] <- valuesmetaKMyear[1] 
            metaLambdasAllIters[pop,iter,firstSeason,flowpert,temppert] <- metaLambdas[pop,firstSeason,flowpert,temppert] 
          }
          if (zeroCond==T){
            metaLambdas[pop] <- valuesmetaKMyear[1] 
          } 

          
       
      }
    }
  }
}


