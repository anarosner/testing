
if (splitStreams==T){
  npop <- 1+nIsolatedSmall
}

if (splitStreams==F){
  npop <- 1
}

source("Code/Scripts/CreateResultsMatricesAllStreamSeason.R")




beforetime <- Sys.time()
for (pop in 1:npop){
  
  source("Code/Scripts/RiverDims.R")               #This determines the structure of the KM matrices for each population and is the main script that makes it expandable to different numbers of rivers
  source("Code/Scripts/Movement.R")                #Creates the movement matrices. Right not is not function of flow or temp, so is outside loops
  source("Code/Scripts/Demographic Functions.R")
  

    for (temppert in 1:(length(tvariables))){
      for (flowpert in 1:(length(fvariables))){
        
        if (pop>1){
          pertriver=4
        }
     
        
        if (zeroCond==F){
          #This inputs the standardized variable values for each stream and season for flow and temp. Can be inputed directly from web site   
          demoVariables <- array(0,c((nOpenLargeMain + nOpenLarge + nOpenSmall + nIsolatedSmall),4,2))
          dimnames(demoVariables) <- list(c(1:(nOpenLargeMain + nOpenLarge + nOpenSmall + nIsolatedSmall)),c("Spring","Summer","Fall","Winter"),c("Flow","Temp"))
          #For now, can be changed using this code. BY default, all values are now zero, which is the mean.
          #demoVariables[3,1,2] <- 10
          demoVariables[,,1] <- fvariables[flowpert]
          demoVariables[,,2] <- tvariables[temppert]
        }

          source("Code/Scripts/ConstCompMatricesAllStreamSeason.R")
          source("Code/Scripts/MetaKM.R")
          
          if (Analytical==F){
            source("Code/Scripts/EigCalcNumerical.R")
          }
          if (Analytical==T){
            source("Code/Scripts/EigCalcAnalytical.R") 
          }   
        
          #yearly lambda
          if (zeroCond==F){
            metaLambdas[pop,flowpert,temppert] <- valuesmetaKMyear[1] 

          }
          if (zeroCond==T){
            metaLambdas[pop] <- valuesmetaKMyear[1] 
          } 
          
 
          if (pop==1){ 
 
            if (zeroCond==T){
              MSensMain <-  MSensSeason
              MElasMain <-  MSensSeason
              KSensMain <- KSensSeason
              KElasMain <- KSensSeason
              
              vitalSensMain <- pDemoSensSeason
              vitalElasMain <- pDemoSensSeason
              
            }
          }
          
          if (pop>1){

            if (zeroCond==T){
              MSensIso[(pop-1),,,] <-  MSensSeason
              MElasIso[(pop-1),,,] <-  MSensSeason
              KSensIso[(pop-1),,,] <- KSensSeason
              KElasIso[(pop-1),,,] <- KSensSeason
              for (rate in 1:6){
                vitalSensIso[(pop-1),,,,] <- pDemoSensSeason
                vitalElasIso[(pop-1),,,,] <- pDemoSensSeason
              }
            }
          }
          
          cat(pop,flowpert,temppert)
       
      }
    }
  }

donetime <- Sys.time()
print(donetime - beforetime)
