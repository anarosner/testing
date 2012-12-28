

Analytical <- T             #Choose whether to find the lambdas and sensitivities 
firstSeason <- 0            #This just turns something off in the code so that I did not have to write different code for the retro analysis
OlberOff <- T               #Turn Migration to Olber off?


source("Scripts/Demographic Functions.R")
source("Scripts/ConstCompMatrices.R")
source("Scripts/MetaKM.R")
if (Analytical==F){
  source("Scripts/EigCalcNumerical.R")
}
if (Analytical==T){
  source("Scripts/EigCalcAnalytical.R") 
}   
source("Scripts/SensElas.R")            
source("Scripts/SensVitalRates.R")

valuesmetaKMyearinitial <- valuesmetaKMyear[1]



pDemoSensSeason[5,3,1,20]                   #rate,season,,

delta <- 0.0001

F3x[1,19,3,1] <-  F3x[1,19,3,1]+delta        #,,season,river


source("Scripts/MetaKM.R")
if (Analytical==F){
  source("Scripts/EigCalcNumerical.R")
}
if (Analytical==T){
  source("Scripts/EigCalcAnalytical.R") 
}


(valuesmetaKMyear[1]-valuesmetaKMyearinitial)/(delta)












#####This is to check growth and recruit size


Analytical <- T             #Choose whether to find the lambdas and sensitivities 
firstSeason <- 0            #This just turns something off in the code so that I did not have to write different code for the retro analysis
OlberOff <- T               #Turn Migration to Olber off?


source("Scripts/Demographic Functions.R")
source("Scripts/ConstCompMatrices.R")
source("Scripts/MetaKM.R")
if (Analytical==F){
  source("Scripts/EigCalcNumerical.R")
}
if (Analytical==T){
  source("Scripts/EigCalcAnalytical.R") 
}   
source("Scripts/SensElas.R")            
source("Scripts/SensVitalRates.R")

valuesmetaKMyearinitial <- valuesmetaKMyear[1]



sum(pDemoSensSeason[1,,,])                  #rate,season,,

delta <- 0.0001

        #,,season,river


gxy<-function(x,y,flow,temp,season,river,params) { 
  if (firstSeason == 0){
    fl  <- 0
    te <- 0
  }
  
  else {
    if (river==pertriver){  
      
      if (season == firstSeason){
        fl  <- flow
        te <- temp
      }
      else {
        fl <- 0
        te <- 0
      }
    }
    else {
      fl <- 0
      te <- 0
    }
  }
  stdX <- (x-params$lengthMean[season,river])/params$lengthSd[season,river]  
  g  <- ( mean(params$grBeta[1,season,river,1:nIter])+delta +
    mean(params$grBeta[2,season,river,1:nIter])*stdX +
   
    mean(params$grBeta[3,season,river,1:nIter])*fl +
    mean(params$grBeta[4,season,river,1:nIter])*te +
    mean(params$grBeta[5,season,river,1:nIter])*fl*te +
    mean(params$grBeta[6,season,river,1:nIter])*fl*stdX +
    mean(params$grBeta[7,season,river,1:nIter])*te*stdX +
    mean(params$grBeta[8,season,river,1:nIter])*fl*te*stdX 
  )
  var <- mean((params$grSigma[season,river,1:nIter])^2)
  sd <- var^(1/2)
  
  return(dnorm(y,g+x,sd))
}
source("Scripts/ConstCompMatrices.R")
source("Scripts/MetaKM.R")
if (Analytical==F){
  source("Scripts/EigCalcNumerical.R")
}
if (Analytical==T){
  source("Scripts/EigCalcAnalytical.R") 
}


(valuesmetaKMyear[1]-valuesmetaKMyearinitial)/(delta)

