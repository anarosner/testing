
if (splitStreams==T){
  npop <- 1+nIsolatedSmall
}

if (splitStreams==F){
  npop <- 1
}

if (zeroCond==T){
  #These are for controlling the flow and temperature variables in the subsequent analyses
  #First one for prospective analyses. You can choose at which flow and temp value in the "demoVariables" array.
  
  nPertRiver <- 1
  nRiver <- nOpenLargeMain + nOpenLarge + nOpenSmall + nIsolatedSmall
  fvariables <- seq(0,0,by=0)
  tvariables <- seq(0,0,by=0)
  nSeason <- 1    #This is not the number of seasons, but is the number of seasons to change the flow and temp in
  
  # demoVariables <- array(0,c((nOpenLargeMain + nOpenLarge + nOpenSmall + nIsolatedSmall),4,2))
  # dimnames(demoVariables) <- list(c(1:(nOpenLargeMain + nOpenLarge + nOpenSmall + nIsolatedSmall)),c("Spring","Summer","Fall","Winter"),c("Flow","Temp"))
  demoVariables <- array(0,c(4,4,2))
  dimnames(demoVariables) <- list(c(1:4),c("Spring","Summer","Fall","Winter"),c("Flow","Temp"))
  #For now, can be changed using this code. BY default, all values are now zero, which is the mean.
  demoVariables[,Season,1] <- Flow        # structure: demoVariables[pertriver,firstSeason,flow]
  demoVariables[,Season,2] <- Temp        # structure: demoVariables[pertriver,firstSeason,temp]
  
}


source(paste0(code_dir,"/Scripts/CreateResultsMatrices.R"))     #Creates arrays to store results


beforetime <- Sys.time()

for (pop in 1:npop){

  source(paste0(code_dir,"/Scripts/RiverDims.R"))               #This determines the structure of the KM matrices for each population and is the main script that makes it expandable to different numbers of rivers
  source(paste0(code_dir,"/Scripts/Movement.R"))                #Creates the movement matrices. Right not is not function of flow or temp, so is outside loops
  source(paste0(code_dir,"/Scripts/Demographic Functions.R"))
  
for (pertriver in 1:nRiver){
  for (firstSeason in 1:nSeason){
    for (temppert in 1:(length(tvariables))){
      for (flowpert in 1:(length(fvariables))){
        
        if (pop>1){
          pertriver=4
        }
        
        
        
        if (zeroCond==F){
          #This inputs the standardized variable values for each stream and season for flow and temp. It will change the flow and temp with each iteration.  
          demoVariables <- array(0,c((nOpenLargeMain + nOpenLarge + nOpenSmall + nIsolatedSmall),4,2))
          dimnames(demoVariables) <- list(c(1:(nOpenLargeMain + nOpenLarge + nOpenSmall + nIsolatedSmall)),c("Spring","Summer","Fall","Winter"),c("Flow","Temp"))
          demoVariables[pertriver,firstSeason,1] <- fvariables[flowpert]
          demoVariables[pertriver,firstSeason,2] <- tvariables[temppert]
        }

          source(paste0(code_dir,"/Scripts/ConstCompMatrices.R"))
          source(paste0(code_dir,"/Scripts/MetaKM.R"))
        
          if (Analytical==F){
            source(paste0(code_dir,"/Scripts/EigCalcNumerical.R"))
          }
          if (Analytical==T){
            source(paste0(code_dir,"/Scripts/EigCalcAnalytical.R")) 
          }   
          
          if (zeroCond==T){
            source(paste0(code_dir,"/Scripts/SensElas.R"))
#            source(paste0(code_dir,"/Scripts/SensVitalRateswithprobrepro.R"))
#            source(paste0(code_dir,"/Scripts/SensFlowTemp.R"))
          }
          
          #yearly lambda
          
          if (zeroCond==F){
            metaLambdas[pop,pertriver,firstSeason,flowpert,temppert] <- valuesmetaKMyear[1] 
#             for(r in 1:2){
#             envsens[pop,pertriver,env,r,firstSeason,flowpert,temppert] <- sum(pEnvSensSeason[env,r,season,,])
#             }
          }
          if (zeroCond==T){
            metaLambdas[pop] <- valuesmetaKMyear[1] 
          } 
          
                    if (Analytical==F){            
                      if (zeroCond==F){
                        if (splitStreams==T){ 
                          if (pop==1){
                            riverLamdasmain[river,firstSeason,flowpert,temppert,] <- riverlam[1,]                      
                          }
                          if (pop>1){
                          riverLamdasIso[(pop-1),river,firstSeason,flowpert,temppert,] <- riverlam[1,]
                          }
                        }
                        riverLamdasmain[river,firstSeason,flowpert,temppert,] <- riverlam[1,]
                      }
                      
                      if (zeroCond==T){
                        if (splitStreams==T){
                          if (pop==1){
                            riverLamdasmain <- riverlam[1,]                      
                          }
                          if (pop>1){
                            riverLamdasIso[(pop-1),] <- riverlam[1,]
                          }
                        }                        
                      }   
                    }
        
          if (pop==1){ 
            #             if (zeroCond==F){
            #             MSensMain[pertriver,firstSeason,flowpert,temppert,,] <-  MSensSeason[season,,]
            #             MElasMain[pertriver,firstSeason,flowpert,temppert,,] <-  MSensSeason[season,,]
            #             KSensMain[pertriver,firstSeason,flowpert,temppert,,] <- KSensSeason[season,,]
            #             KElasMain[pertriver,firstSeason,flowpert,temppert,,] <- KSensSeason[season,,]
            #             for (rate in 1:6){
            #               vitalSensMain[rate,pertriver,firstSeason,flowpert,temppert,,] <- pDemoSensSeason[rate,firstSeason,,]
            #               vitalElasMain[rate,pertriver,firstSeason,flowpert,temppert,,] <- pDemoSensSeason[rate,firstSeason,,]
            #               }
            #             }
            if (zeroCond==T){
              MSensMain <-  MSensSeason
              MElasMain <-  MElasSeason
              KSensMain <- KSensSeason
              KElasMain <- KElasSeason
              
              
#               vitalSensMain <- pDemoSensSeason
#               vitalElasMain <- pDemoElasSeason
#               EnvSensMain <- pEnvSensSeason
              vectorsMain <- vectorsmetaKMyear
            }
          }
          
          if (pop>1){
            #             if (zeroCond==F){
            #             MSensIso[(pop-1),pertriver,firstSeason,flowpert,temppert,,] <-  MSensSeason[season,,]
            #             MElasIso[(pop-1),pertriver,firstSeason,flowpert,temppert,,] <-  MSensSeason[season,,]
            #             KSensIso[(pop-1),pertriver,firstSeason,flowpert,temppert,,] <- KSensSeason[season,,]
            #             KElasIso[(pop-1),pertriver,firstSeason,flowpert,temppert,,] <- KSensSeason[season,,]
            #             for (rate in 1:6){
            #               vitalSensIso[(pop-1),rate,pertriver,firstSeason,flowpert,temppert,,] <- pDemoSensSeason[rate,firstSeason,,]
            #               vitalElasIso[(pop-1),rate,pertriver,firstSeason,flowpert,temppert,,] <- pDemoSensSeason[rate,firstSeason,,]
            #               }
            #             }
            if (zeroCond==T){
              MSensIso[(pop-1),,,] <-  MSensSeason
              MElasIso[(pop-1),,,] <-  MElasSeason
              KSensIso[(pop-1),,,] <- KSensSeason
              KElasIso[(pop-1),,,] <- KElasSeason
              vectorsIso <- vectorsmetaKMyear
#               EnvSensIso <- pEnvSensSeason
              for (rate in 1:6){
#                 vitalSensIso[(pop-1),,,,] <- pDemoSensSeason
#                 vitalElasIso[(pop-1),,,,] <- pDemoElasSeason
              }
            }
          }
          
          cat(pop,pertriver,firstSeason,flowpert,temppert)
        }
      }
    }
  }
}
donetime <- Sys.time()
print(donetime - beforetime)

