setwd(paste0(basedir,"/r/models/IPM"))


#============================================================================================#
# (IV) This runs the Analysis to get results at flow and temp = 0 to get the starting stable size distributions
#============================================================================================#
if(initialSizeObs==F){
  
  Analytical <- T
  zeroCond <- T                   
  allStreamPerturb <- F           
  allStreamSeasonPerturb  <- F
  
  nPertRiver <- 1
  nRiver <- nOpenLargeMain + nOpenLarge + nOpenSmall + nIsolatedSmall
  fvariables <- seq(0,0,by=0)
  tvariables <- seq(0,0,by=0)
  nSeason <- 1    #This is not the number of seasons, but is the number of seasons to change the flow and temp in
  
  demoVariables <- array(0,c(4,4,2))
  dimnames(demoVariables) <- list(c(1:4),c("Spring","Summer","Fall","Winter"),c("Flow","Temp"))
  
  ##This runs the basic analysis
  source("Code/Scripts/IPM Run.R")
  
  
  if (splitStreams==T){
    brookAnalysis <- list(metaLambdas=metaLambdas,vectorsMain=vectorsMain,vectorsIso=vectorsIso#,MSensMain=MSensMain,MSensIso=MSensIso,KSensMain=KSensMain,vitalSensMain=vitalSensMain,vitalSensIso=vitalSensIso,
                          #MElasMain=MElasMain,MElasIso=MElasIso,KElasMain=KElasMain,vitalElasMain=vitalElasMain,vitalElasIso=vitalElasIso
                          #,EnvSensMain=EnvSensMain,EnvSensIso=EnvSensIso
                          )
  }
  
  if (splitStreams==F){
    brookAnalysis <- list(metaLambdas=metaLambdas,MSensMain=MSensMain,KSensMain=KSensMain,vitalSensMain=vitalSensMain,
                          #MElasMain=MElasMain,KElasMain=KElasMain,vitalElasMain=vitalElasMain
                          #,EnvSensMain=EnvSensMain
                          )
  }

}

#============================================================================================#
# (VI) This runs the Analysis to get results from the stochastic analysis
#============================================================================================#



zeroCond <- F

if (splitStreams==T){
  npop <- 1+nIsolatedSmall
}

if (splitStreams==F){
  npop <- 1
}

if (means == F){
  popSize <- array(0,c(npop,nIter,years+1,3))
  NDistMain <- array(0,c(nIter,years+1,3,3*(1+length(y))))
  NDistIso <- array(0,c(npop,nIter,years+1,3,1*(1+length(y))))
  
  stochLamb1 <- array(0,c(npop,nIter,years))
  stochLamb2 <- array(0,c(npop,nIter,years))
  yearlyLamb <- array(0,c(npop,nIter,years))
}

if (means == T){
  popSize <- array(0,c(npop,years+1,3))
  NDistMain <- array(0,c(years+1,3,3*(1+length(y))))
  NDistIso <- array(0,c(npop,years+1,3,1*(1+length(y))))
  
  stochLamb1 <- array(0,c(npop,years))
  stochLamb2 <- array(NA,c(npop,years))
  yearlyLamb <- array(NA,c(npop,years))
}


beforetime <- Sys.time()
for (pop in 1:npop){
  
  source("Code/Scripts/RiverDims.R")
  
  if (means==F){ 
  source("Code/Scripts/Demographic Functions All Iters.R")
  for (iter in 1:nIter){
    yr=1
    source("Code/Scripts/Movement All Iters.R")                
    source("Code/Scripts/IPM Run All Stream PerturbStoch.R")
    }
  }
  if (means==T){
    source("Code/Scripts/Demographic Functions.R")
      yr=1
      source("Code/Scripts/Movement.R")                
      source("Code/Scripts/IPM Run All Stream PerturbStoch.R")
    }
}

donetime <- Sys.time()
print(donetime - beforetime)

setwd(directory)
save.image("StochProj.RData")
# save.image("Output/StochProj.RData")

