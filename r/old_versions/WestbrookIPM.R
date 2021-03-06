
#clear everything, just to be safe 
#rm(list=ls(all=TRUE))

setwd('/home/austin/WebStuff/')

load('Data/InputData/outSjSurv.RData')
#load('Data/InputData/outPreTagLength.RData')
load('Data/InputData/outPreTagMean.RData')
load('Data/InputData/outMSRiver.RData')


#============================================================================================#
# (I) Controls for the number of interations to be run, kernel size, number of rivers, etc
#============================================================================================#

# minimum and maximum sizes 
minsize= 0 
maxsize=300 
L= minsize 
U= maxsize
nIter <- 200
eggSize <- 1              #Do not change. This defines the number of sub-"adult" stages, not yet setup to accomodate more
n <- n.big.matrix <- 70   #size of demographic matrix
probReproInt <- 90
probReproSlope <- 1

#These can be changed to reflect different numbers of rivers
nOpenLargeMain <- 1
nOpenLarge <- 1
nOpenSmall <- 1
nIsolatedSmall <- 1
riverType <- 4            #Do not change



# boundary points b and mesh points y
b = L+c(0:n)*(U-L)/n; 
y = 0.5*(b[1:n]+b[2:(n+1)]);

# step size for midpoint rule
h = y[2]-y[1]

#These are the stream areas in different seasons
areas <- array(c(7851.197224 , 1047.67033,  683.7244898,  965.5,
                 6506.815146,  892.3333333,  563.2346939,  817.1868132,
                 5421.365353,  715.1666667,	390.0878571,	823.5,
                 7253.35262,	985.8,	676.9285714,	966.6666667),c(4,4))
# areas <- array(1,c(4,4))

areas <- t(areas)
areas <- areas[,1:3]

riverArea <- array(0,c(4,(length(y)+1)*3,(length(y)+1)*3))
for (season in 1:4){
  riverArea[season,,] <- areas[season,]%x%array(1,c(length(y)+1,1))
  riverArea[season,,] <- diag(1,(length(y)+1)*3,(length(y)+1)*3)*riverArea[season,,]
}



#============================================================================================#
# (II) Input parameters from MCMC models of vital rates
#============================================================================================#
source('Code/Scripts/ImportDataAllIters.R')           #This can be any data, such as those changed by the website

#============================================================================================#
# (III) Analysis specific controls
#============================================================================================#
##There are two general types of analyses that can be done. 1) stochastic and 2) deterministic. 
##Within the deterministic we can do either a prospective or retrospective analysis. Only the prospective is set up here right now. 
stochastic <- T
  if (stochastic==T)  {
    years <- 10                   #or can input number from 1-bazilliions. If you choose the weather generator option below, this will automatically get changed to 80. Westbrook will be 8
    quasiextinct <- 10
    EnvVarsFromWeatherGen <- T
    EnvVarsFromObsWestBrook <- F
    EnvVarsRandomDraw <- F
    initialSizeObs <- T    
    means <- T
    Prospective <- F
    ##Do not change these below

    Flow <- 0
    Temp <- 0
    Season <- 1
    zeroCond <- F
  }

deterministic <- F
  if (deterministic == T)  {
    Analytical <- T                 #Choose whether to find the lambdas and sensitivities Analytically or through Numerical iteration
      Prospective <- T                
        Posterior <- T                #Choose whether to calculate the posterior distribution of Lambda
      if (Posterior==T){means <- F}
      if (Posterior==F){means <- T}
        Flow <- 0                     #All seasonal flows and temps will be zero unless changed here
        Temp <- 0
        Season <- c(1,2,3,4)                   #Season that the flow and temp will apply
      Retrospective <- F
        allStreamPerturb <- F           #If T, then analysis will be done that changes temp and flow in all streams, one season at a time
        allStreamSeasonPerturb  <- F    #If T, then analyses will be done that changes temp and flow in all streams for all seasons at the same time

	#This is to keep you from screwing up the controls too much
	if (Prospective==T){zeroCond <- T}
	}


move <- T                       #Turn to T to allow movement between rivers

#These controls deal with whether you want the isolated stream to be isolated or connected
ObearOff <- F                   #Turn Migration to Olber off? THis must be turned off if move=F
splitStreams <- T               #This is for splitting up the streams into the three connected and one isolated populations. 

if (move==F){Obearoff=F}
if (zeroCond==T){
  allStreamPerturb=F
  allStreamSeasonPerturb=F
}


#================================================================================================================================
#IV This runs the stochastic analysis and spits out graphs
#================================================================================================================================
if (stochastic==T){
  source("Code/Scripts/ChooseEnvType.R")
  source("Code/Scripts/Stochastic Analysis.R")
  
  stochLambda  
  
  ###These are the graphics
if (EnvVarsFromObsWestBrook==T){ 
  if (means==T){
    source("Code/Graphics/Stochastic ObsWest Means Ad v Recruit.R")
  }
  if (means==F){
    source("Code/Graphics/StochLambdawithPosterior.R") 
    source("Code/Graphics/Stochastic ObsWest.R")
    source("Code/Graphics/QuasiExtinction.R")
  }
}
  
  
 #This part is not yet working
 if (EnvVarsFromWeatherGen==T){ 
   if (means==T){
   source("Code/Graphics/Stochastic Weath Gen Means.R")
   }
 }
   
   
  if(EnvVarsRandomDraw==T){
    source("Code/Graphics/Stochastic Weath Gen Means.R")
  }
  
  
  
  
  if (EnvVarsFromWeatherGen==T){
    save.image("Output/Stochastic/Means with Posterior/StochWeatherGen.RData")
  }
  
  if (EnvVarsFromObsWestBrook==T){
    save.image("Output/Stochastic/Means with Posterior/StochWestBrook.RData")
  }
  
  if (EnvVarsRandomDraw==T){
    save.image("Output/Stochastic/Means with Posterior/StochRandom.RData")
  } 
  
}
