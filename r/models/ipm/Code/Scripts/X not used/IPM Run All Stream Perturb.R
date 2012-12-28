
if (splitStreams==T){
  npop <- 1+nIsolatedSmall
}

if (splitStreams==F){
  npop <- 1
}


source("Code/Scripts/CreateResultsMatricesAllStreams.R")


beforetime <- Sys.time()


for (pop in 1:1){
  
  source("Code/Scripts/RiverDims.R")
  source("Code/Scripts/Movement.R")                #Creates the movement matrices. Right not is not function of flow or temp, so is outside loops
  source("Code/Scripts/Demographic Functions.R")


  for (firstSeason in 1:nSeason){
    for (yr in 1:5){
          tempert <- flowpert <- yr
          
        if (zeroCond==F){
          #This inputs the standardized variable values for each stream and season for flow and temp. Can be inputed directly from web site   
          demoVariables <- array(0,c((nOpenLargeMain + nOpenLarge + nOpenSmall + nIsolatedSmall),4,2))
          dimnames(demoVariables) <- list(c(1:(nOpenLargeMain + nOpenLarge + nOpenSmall + nIsolatedSmall)),c("Spring","Summer","Fall","Winter"),c("Flow","Temp"))
          #For now, can be changed using this code. BY default, all values are now zero, which is the mean.
          #demoVariables[3,1,2] <- 10
          for (river in 1:4){
            demoVariables[river,,1] <- streamVars[((yr*4)-3):(4*yr),7]
          }
          demoVariables[,,2] <- t(streamVars[((yr*4)-3):(4*yr),3:6])
        }
  
          source("Code/Scripts/ConstCompMatricesAllStreamsStoch.R")
          source("Code/Scripts/MetaKM.R")
          
          if (yr==1){
            metaKMlong <- metaKMyear[1,,]
            if (Analytical==T){
              source("Code/Scripts/EigCalcAnalytical.R") 
            }
            if (Analytical==F){
              source("Code/Scripts/EigCalcNumerical.R")
            }
            
            valuegeomean <- valuesmetaKMyear
            
          }
          if (yr>1){
            metaKMlong <- metaKMyear[1,,]%*%metaKMlong
            valuegeomean <- valuegeomean*valuesmetaKMyear
          }
 
    }
  }
}
donetime <- Sys.time()
print(donetime - beforetime)

if (Analytical==F){
  source("Code/Scripts/EigCalcNumerical.R")
}
if (Analytical==T){
  source("Code/Scripts/EigCalcAnalyticalstoch.R") 
}

valuegeomean^(1/years)