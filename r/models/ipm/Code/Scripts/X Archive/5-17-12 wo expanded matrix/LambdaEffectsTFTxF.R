

effectLambEnvSeasRate <- array(NA,c(nRiver,4,length(fvariables),length(tvariables),6,4,length(y)*nRiver,length(y)*nRiver))
effectLambEnvSeas <- array(NA,c(nRiver,4,length(fvariables),length(tvariables),6,4,length(y)*nRiver,length(y)*nRiver))
effectLambEnvSeasSum <- array(NA,c(nRiver,4,length(fvariables),length(tvariables),4,length(y)*nRiver,length(y)*nRiver))
effectLambEnvYearSum <- array(NA,c(nRiver,4,length(fvariables),length(tvariables)))
perturblambdas <- array(NA,c(nRiver,4,length(fvariables),length(tvariables)))


for (pertriver in 1:nRiver){
  for (firstSeason in 1:4){
  for (temppert in 1:(length(tvariables))){
    temp <- tvariables[temppert]
    for (flowpert in 1:(length(fvariables))){
      flow <- fvariables[flowpert]
      source("Scripts/Demographic Functions.R")
      source("Scripts/ConstCompMatrices.R")
      source("Scripts/MetaKM.R")
      source("Scripts/EigCalcAnalytical.R")    
      source("Scripts/SensElas.R")
      source("Scripts/SensVitalRates.R")
      source("Scripts/SensFlow.R")
      source("Scripts/SensTemp.R")
      source("Scripts/SensFlowTemp.R")
      perturblambdas[pertriver,firstSeason,flowpert,temppert] <- valuesmetaKMyear[1]  
      for (season in 1:4){
        for (rate in 1:6){
          effectLambEnvSeasRate[pertriver,firstSeason,flowpert,temppert,rate,season,,] <- (KSensSeason[season,,] * metaPdKyxVr[rate,season,,] * metaPdDemoFlow[rate,season,,] 
                                                                               + KSensSeason[season,,] * metaPdKyxVr[rate,season,,] * metaPdDemotemp[rate,season,,] 
                                                                               + KSensSeason[season,,] * metaPdKyxVr[rate,season,,] * metaPdDemoFxT[rate,season,,] )    
        }
        effectLambEnvSeas[pertriver,firstSeason,flowpert,temppert,season,,] <- (effectLambEnvSeasRate[pertriver,firstSeason,flowpert,temppert,1,season,,]+
          effectLambEnvSeasRate[pertriver,firstSeason,flowpert,temppert,2,season,,]+
          effectLambEnvSeasRate[pertriver,firstSeason,flowpert,temppert,3,season,,]+
          effectLambEnvSeasRate[pertriver,firstSeason,flowpert,temppert,4,season,,]+
          effectLambEnvSeasRate[pertriver,firstSeason,flowpert,temppert,5,season,,]+
          effectLambEnvSeasRate[pertriver,firstSeason,flowpert,temppert,6,season,,])
        
        effectLambEnvSeasSum[pertriver,firstSeason,flowpert,temppert,season] <- sum(effectLambEnvSeas[pertriver,firstSeason,flowpert,temppert,season,,])
      }
      
      effectLambEnvYearSum[pertriver,firstSeason,flowpert,temppert] <- sum(effectLambEnvSeasSum[pertriver,firstSeason,flowpert,temppert,])
    }
  }
 }
}