
if (EnvVarsFromWeatherGen==T){
######This is for importing the flow and temp data from the weather generator#################

# short.dir <- gsub("./","",directory)
# dir.stoc1 <- paste("/home/austin/WebStuff/Images/",short.dir,"/seasonal_streamtemp.txt",sep="")
# dir.stoc2 <- paste("/home/austin/WebStuff/Images/",short.dir,"/Seasonal_Streamflow_Westbrook.txt",sep="")

#Read in streamflow generated in preceding run
setwd(preceding$flow_dir)
streamFlow <- read.delim("Seasonal_Streamflow.txt", header=F, sep = "",quote="")
streamFlow <-as.matrix(streamFlow)

#Read in streamflow generated in preceding run
setwd(preceding$streamtemp_dir)
streamTemp <- read.delim("seasonal_streamtemp.txt", header=F, sep = "",quote="")
streamTemp <-as.matrix(streamTemp)

streamVars <- array(NA,c(324,7))
streamVars[,1:6] <- streamTemp
streamVars[,7] <- streamFlow[,3]

#This standardizes the weather generator data to itself
e <- array(0,c(4,5))
stan <- array(0,c(4,5))
for (season in 1:4){
  for(column in 3:7){
    e[season,(column-2)] <- (sum(streamVars[which(streamVars[,2]==season),column])/81)
    stan[season,(column-2)] <- sd(streamVars[which(streamVars[,2]==season),column])
  }
}

for (season in 1:4){
  for(column in 3:7){
    streamVars[which(streamVars[,2]==season),column] <- (streamVars[which(streamVars[,2]==season),column]-e[season,(column-2)])/stan[season,(column-2)]
  }
}

streamVars2 <-  streamVars[,7]
streamVars <- abind(streamVars,streamVars2,streamVars2,streamVars2,along=2)
streamVars <- streamVars[-c(1,2,323,324),]

years <- (dim(streamVars)[1]/4)
}


if (EnvVarsFromObsWestBrook==T){
  years <- 8
  
  # means for standardizing 
  #####################################################################  
  stdBySeasonRiver <- ddply( dMData, .(riverOrdered,riverN,season), summarise,   
                             lengthMean=mean(length, na.rm=TRUE),                       
                             lengthSd=sd(length, na.rm=TRUE),
                             lengthLo = quantile(length,c(0.025), na.rm=TRUE),
                             lengthHi = quantile(length,c(0.975), na.rm=TRUE),
                             tempMean=mean(fullMeanT, na.rm=TRUE),
                             tempMeanP=mean(temperatureForP, na.rm=TRUE), 
                             tempSd=sd(fullMeanT, na.rm=TRUE),
                             tempSdP=sd(temperatureForP, na.rm=TRUE),
                             tempLo = quantile(fullMeanT,c(0.025), na.rm=TRUE),
                             tempHi = quantile(fullMeanT,c(0.975), na.rm=TRUE),
                             flowMean=mean(fullMeanD, na.rm=TRUE), 
                             flowSd=sd(fullMeanD, na.rm=TRUE),
                             dischMeanP=mean(dischargeForP,na.rm=T),
                             dischSdP=sd(dischargeForP,na.rm=T),
                             flowLo = quantile(fullMeanD,c(0.025), na.rm=TRUE),
                             flowHi = quantile(fullMeanD,c(0.975), na.rm=TRUE) )
  ############# To get rid of NA Rivers
  stdBySeasonRiver<-stdBySeasonRiver[!is.na(stdBySeasonRiver$riverN),]
  
  
  
  
  
  tempForN<- array(NA,dim=c(4,5,max(dMData$year-min(dMData$year) + 1)))
  for(s in 1:4){            
    for(ye in 1:max(dMData$year-min(dMData$year) + 1)){
      tempForN[s,1,ye]<-0
      for(r in 1:4){
        tempForN[s,r+1,ye]<-(mean(dMData$fullMeanT[dMData$season==s&as.numeric(dMData$riverOrdered)==r&(dMData$year-min(dMData$year) + 1)==ye],na.rm=T)
                            - stdBySeasonRiver$tempMean[ 4*(r-1)+s ] ) / stdBySeasonRiver$tempSd[ 4*(r-1)+s ]
        if(tempForN[s,r+1,ye]=='NaN')tempForN[s,r+1,ye]<-(stdBySeasonRiver$tempMean[4*(r-1)+s]- stdBySeasonRiver$tempMean[ 4*(r-1)+s] ) / stdBySeasonRiver$tempSd[ 4*(r-1)+s ]
      }
    }
  }
  
  
  flowForN<- array(NA,dim=c(4,5,max(dMData$year-min(dMData$year) + 1)))
  for(s in 1:4){            
    for(ye in 1:max(dMData$year-min(dMData$year) + 1)){
      flowForN[s,1,ye]<-0
      for(r in 1:4){
        flowForN[s,r+1,ye]<-(mean(dMData$fullMeanD[dMData$season==s&as.numeric(dMData$riverOrdered)==r&(dMData$year-min(dMData$year) + 1)==ye],na.rm=T)
                            - stdBySeasonRiver$flowMean[4*(r-1)+s] ) / stdBySeasonRiver$flowSd[4*(r-1)+s]
        if(flowForN[s,r+1,ye]=='NaN')flowForN[s,r+1,ye]<-(stdBySeasonRiver$flowMean[4*(r-1)+s]- stdBySeasonRiver$flowMean[4*(r-1)+s] ) / stdBySeasonRiver$flowSd[4*(r-1)+s]
      }
    }
  }
  
  
  env <- cbind(melt(flowForN), melt(tempForN))
#  env <- cbind(melt(d$flowForN), melt(d$tempForN))
  env <- env[,-(5:7)]
  names(env) <- c('season','river2','year2','stdF','stdT')
  
  env$year <- env$year2 + d$minYear - 1
  
  
  env <- env[env$river2 != 1,] ; env$river2 <- as.numeric(as.character(env$river2)) - 1
  env$river <- 'WEST BROOK'
  env$river[env$river2 == 2] <- 'WB JIMMY'
  env$river[env$river2 == 3] <- 'WB MITCHELL'
  env$river[env$river2 == 4] <- 'WB OBEAR'
  
  env$riverOrdered <- factor(env$river,levels=c('WEST BROOK','WB JIMMY','WB MITCHELL','WB OBEAR'), ordered=T)
  
  env <- env[order(env$riverOrdered,env$year,env$season),]
  
  
  MeansBySeasonRiverYear <- abind(env[which(env$river=='WEST BROOK'),c('year','season','stdT')],env[which(env$river=='WB JIMMY'),c('stdT')],
                                  env[which(env$river=='WB MITCHELL'),c('stdT')],env[which(env$river=='WB OBEAR'),c('stdT')],
                                  env[which(env$river=='WEST BROOK'),c('stdF')],env[which(env$river=='WB JIMMY'),c('stdF')],
                                  env[which(env$river=='WB MITCHELL'),c('stdF')],env[which(env$river=='WB OBEAR'),c('stdF')], along=2)
  
  MeansBySeasonRiverYear <- MeansBySeasonRiverYear[-c(c(1:6),c(39:44)),]
  
#   years <- 8
#   
#   ##This is for the observed Temps and Flows
#   MeansBySeasonRiverYear <- read.delim("C:/Users/rbassar/Documents/Projects/Current/Westbrook/Stream MetaData/Mean Seasonal Temps and Flows.txt", header=F, quote="")
#   
#   meanTemp<- array(  c(10.551238,16.0623,8.372859,1.812888,10.212622,15.910017,8.049415,1.740035,9.688889,15.987559,7.938804,1.747967,10.582724,16.022987,7.687613,1.723267)  ,c(4,4))
#   sdTemp<- array(  c(1.0998522,0.6199532,0.9626596,0.5391544,1.3545159,0.6132194,1.6494288,0.5117662,1.2531316,0.5531685,1.4912163,0.4937072,1.3247048,0.6608463,1.9531092,0.4540174)  ,c(4,4))
#   meanFlow<- array(	c(0.15166295,0.05300424,0.13661979,0.19595784,0.17194322,0.0527205,0.15803548,0.19982652,0.18223888,0.04559319,0.1395012,0.20733059,0.16492763,0.05221807,0.15686571,0.20161237)	,c(4,4))
#   sdFlow<- array(	c(0.10286474,0.04629517,0.09516721,0.04057999,0.08104088,0.04281797,0.11433226,0.04689907,0.0668225,0.03685448,0.10349906,0.03534362,0.08026413,0.04588228,0.11846863,0.04350088)	,c(4,4))
#   
#   dimnames(meanTemp) <- list(c("Spring","Summer","Fall","Winter"),c("WestBrook","Jimmy","Mitchell","Obear"))
#   dimnames(sdTemp) <- list(c("Spring","Summer","Fall","Winter"),c("WestBrook","Jimmy","Mitchell","Obear"))
#   dimnames(meanFlow) <- list(c("Spring","Summer","Fall","Winter"),c("WestBrook","Jimmy","Mitchell","Obear"))
#   dimnames(sdFlow) <- list(c("Spring","Summer","Fall","Winter"),c("WestBrook","Jimmy","Mitchell","Obear"))
#   
#   e <- array(NA,c(36,4))
#   a <- MeansBySeasonRiverYear[,c(2,7:10)]
#   for (season in 1:4){
#     for (river in 1:4){
#       e[which(a[,1]==season),river] <- (a[which(a[,1]==season),(river+1)]-meanFlow[season,river])/sdFlow[season,river]
#       
#     }
#   }
#   
#   MeansBySeasonRiverYear[,c(7:10)] <- e[,1:4]
#   
#   e <- array(NA,c(36,4))
#   a <- MeansBySeasonRiverYear[,c(2,3:6)]
#   for (season in 1:4){
#     for (river in 1:4){
#       e[which(a[,1]==season),river] <- (a[which(a[,1]==season),(river+1)]-meanTemp[season,river])/sdTemp[season,river]
#       
#     }
#   }
#   
#   MeansBySeasonRiverYear[,c(3:6)] <- e[,1:4]

}





# #These are the standardized values of flow and temp from the observed Westbrook flow and temp data
# meanTemp<- array(  c(10.551238,16.0623,8.372859,1.812888,10.212622,15.910017,8.049415,1.740035,9.688889,15.987559,7.938804,1.747967,10.582724,16.022987,7.687613,1.723267)  ,c(4,4))
# sdTemp<- array(  c(1.0998522,0.6199532,0.9626596,0.5391544,1.3545159,0.6132194,1.6494288,0.5117662,1.2531316,0.5531685,1.4912163,0.4937072,1.3247048,0.6608463,1.9531092,0.4540174)  ,c(4,4))
# meanFlow<- array(	c(0.15166295,0.05300424,0.13661979,0.19595784,0.17194322,0.0527205,0.15803548,0.19982652,0.18223888,0.04559319,0.1395012,0.20733059,0.16492763,0.05221807,0.15686571,0.20161237)	,c(4,4))
# sdFlow<- array(	c(0.10286474,0.04629517,0.09516721,0.04057999,0.08104088,0.04281797,0.11433226,0.04689907,0.0668225,0.03685448,0.10349906,0.03534362,0.08026413,0.04588228,0.11846863,0.04350088)	,c(4,4))
# 
# dimnames(meanTemp) <- list(c("Spring","Summer","Fall","Winter"),c("WestBrook","Jimmy","Mitchell","Obear"))
# dimnames(sdTemp) <- list(c("Spring","Summer","Fall","Winter"),c("WestBrook","Jimmy","Mitchell","Obear"))
# dimnames(meanFlow) <- list(c("Spring","Summer","Fall","Winter"),c("WestBrook","Jimmy","Mitchell","Obear"))
# dimnames(sdFlow) <- list(c("Spring","Summer","Fall","Winter"),c("WestBrook","Jimmy","Mitchell","Obear"))
# 
# ##This standardizes the weather generator flow and temp to the Westbrook data.
# a <- streamVars[,c(2,7)]
# e <- array(NA,c(324,4))
# for (season in 1:4){
#   for(river in 1:4){
#     e[which(a[,1]==season),river] <- (0.028316847*a[which(a[,1]==season),2]-meanFlow[season,1])/sdFlow[season,1]
#   }
# }
# 
# streamVars[,7] <- e[,1];streamVars <- abind(streamVars,e[,c(2:4)],along=2)
# 
# e <- array(NA,c(324,4))
# a <- streamVars[,c(2,3:6)]
# for (season in 1:4){
#   for (river in 1:4){
#     e[which(a[,1]==season),river] <- (a[which(a[,1]==season),(river+1)]-meanTemp[season,river])#/sdTemp[season,river]
#     
#   }
# }
# 
# streamVars[,c(3:6)] <- e[,1:4]
