rm(list=ls(all=TRUE))

library(plyr)
library(rjags)
library(ggplot2)
library(abind)
#rjags::load.module("dic")




####################
load("~/Projects/Current/Westbrook/Brook Trout/Data/Estimation Models/dMDataOutBKT2002_2011.RData")
load('~/Projects/Current/Westbrook/Brook Trout/Data/Estimation Models/earlyLifeInputForBen.RData')
load("~/Projects/Current/Westbrook/Brook Trout/Data/Input Data/outMSRiver.RData")

dMData$length[dMData$tagNumberCH=='1BF1FF6207' & dMData$season == 3 & dMData$year == 2005] <- NA
dMData$length[dMData$tagNumberCH=='1BF1FF6521' & dMData$season == 3 & dMData$year == 2005] <- NA
dMData$length[dMData$tagNumberCH=='1BF18CE7ED' & dMData$season == 2 & dMData$year == 2006] <- NA
dMData$length[dMData$tagNumberCH=='1BF20FF1B9' & dMData$season == 3 & dMData$year == 2005] <- NA
dMData$length[dMData$tagNumberCH=='257C67CA48' ] <- NA
dMData$length[dMData$tagNumberCH=='1BF20EB7A4' & dMData$season == 4 & dMData$year == 2008] <- NA



dMData$riverOrdered <- factor(dMData$river,levels=c('WEST BROOK','WB JIMMY','WB MITCHELL','WB OBEAR'), ordered=T)


dPre <- dMData[ dMData$enc == 1 &
  dMData$season == 3 &
  dMData$age == 0,
                c('tagNumberCH','year','season','riverOrdered','length')]
dPre$yearYOY <- dPre$year

# ggplot(dPre, aes(length))+
#   geom_freqpoly(aes(colour=factor(year)), binwidth=5)+
#   facet_grid(~river)

dPreMean <- ddply( dPre, .(yearYOY,season,riverOrdered), summarise,   
                           lengthMean=mean(length, na.rm=TRUE) )


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
env <- env[,-(5:7)]
names(env) <- c('season','river2','year2','stdF','stdT')

env$year <- env$year2 + d$minYear - 1
env$yearYOY <- env$year + 1

env <- env[env$river2 != 1,] ; env$river2 <- as.numeric(as.character(env$river2)) - 1
env$river <- 'WEST BROOK'
env$river[env$river2 == 2] <- 'WB JIMMY'
env$river[env$river2 == 3] <- 'WB MITCHELL'
env$river[env$river2 == 4] <- 'WB OBEAR'

env$riverOrdered <- factor(env$river,levels=c('WEST BROOK','WB JIMMY','WB MITCHELL','WB OBEAR'), ordered=T)

env <- env[order(env$riverOrdered,env$year,env$season),]

env$stdFLag1 <- c(NA,env$stdF[1:(nrow(env)-1)])
env$stdFLag2 <- c(NA,NA,env$stdF[1:(nrow(env)-2)])
env$stdFLag3 <- c(NA,NA,NA,env$stdF[1:(nrow(env)-3)])
env$stdFLag4 <- c(NA,NA,NA,NA,env$stdF[1:(nrow(env)-4)])
env$stdTLag1 <- c(NA,env$stdT[1:(nrow(env)-1)])
env$stdTLag2 <- c(NA,NA,env$stdT[1:(nrow(env)-2)])
env$stdTLag3 <- c(NA,NA,NA,env$stdT[1:(nrow(env)-3)])
env$stdTLag4 <- c(NA,NA,NA,NA,env$stdT[1:(nrow(env)-4)])



dPreMean2 <- merge(x=dPreMean, y=env[,c('season','year','riverOrdered','stdF','stdT','stdFLag1','stdTLag1','stdFLag2','stdTLag2','stdFLag3','stdTLag3','stdFLag4','stdTLag4')], 
                   by.x=c('season','yearYOY','riverOrdered'), by.y=c('season','year','riverOrdered'),all.x=T)

dPreMean2 <- dPreMean2[-c(1:4),]


dPreMean2$stdFLag12 <- dPreMean2$stdFLag1^2
dPreMean2$stdFLag22 <- dPreMean2$stdFLag2^2
dPreMean2$stdFLag32 <- dPreMean2$stdFLag3^2
dPreMean2$stdFLag42 <- dPreMean2$stdFLag4^2
dPreMean2$stdTLag12 <- dPreMean2$stdTLag1^2
dPreMean2$stdTLag22 <- dPreMean2$stdTLag2^2
dPreMean2$stdTLag32 <- dPreMean2$stdTLag3^2
dPreMean2$stdTLag42 <- dPreMean2$stdTLag4^2


# dPreMean2$survJuv[29] <- NA
# dPreMean2$logitsurvJuv[29] <- NA

mod <- glm( (lengthMean) ~ 
  
  0 +  riverOrdered  +
  
  
  stdTLag1 + 
  stdFLag1 + 
  stdTLag2 + 
  stdFLag2 +
  stdTLag3 + 
  stdFLag3 +
  stdTLag4 + 
  stdFLag4 +  
  
  stdTLag12 + 
  stdFLag12 + 
  stdTLag22 + 
  stdFLag22 +
  stdTLag32 + 
  stdFLag32 + 
  stdTLag42 + 
  stdFLag42    
            
            
            
            , family=gaussian,
            data=dPreMean2)#[dPreMean2$river %in% 'WEST BROOK',]  )
summary(mod)
plot(mod)
# plot(dPreMean2$survJuv,mod$fitted.values)

ggplot(dPreMean2, aes(x=(yearYOY),y=(lengthMean))) +
  geom_line(size=2)+
  geom_text(aes(label=yearYOY)) +
  facet_wrap(~river,scales='free')

ggplot(dPreMean2, aes(x=(year),y=(lengthMean))) +
  geom_line(size=2)+
  geom_text(aes(label=year)) +
  facet_wrap(~river,scales='free')

ggplot(dPreMean2, aes(x=(year),y=(lengthMean))) +
  geom_line(size=2)+
  geom_text(aes(label=year)) +
  facet_wrap(~river,scales='free')

ggplot(dPreMean2, aes(x=(stdTLag1),y=(lengthMean))) +
  geom_point(size=2)+
  geom_text(aes(label=yearYOY)) +
  facet_wrap(~riverOrdered,scales='free')+
  geom_smooth(method = "lm", formula = y ~ poly(x,2), colour = "red")  


outPre <- mod$coefficients


save.image("~/Projects/Current/Westbrook/Brook Trout/Data/Input Data/outPreTagMean.RData")

