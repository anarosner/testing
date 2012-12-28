#clear everything, just to be safe 
rm(list=ls(all=TRUE))


library(ggplot2)
library(plyr)
library(reshape)
library(arm)
library(lme4)

setwd('~/Projects/Current/Westbrook/Brook Trout/Data/Offspring Survival')

#load('C:/PITTAGMAIN/CMR Analyses/Hierach_Bugs/allSpp/dMDataOutBKT2002_2011.RData')
load("~/Projects/Current/Westbrook/Brook Trout/Data/Estimation Models/dMDataOutBKT2002_2011.RData")
####################################################
# make data frame of p data

#load( "c:/PITTAGMAIN/CMR Analyses/Hierach_Bugs/allSpp/MSRiver2/output-39ac22712d23-MSRiver/outMSRiver.RData")
#load("~/Projects/Current/Westbrook/Brook Trout/Data/MSRiver2/output-39ac22712d23-MSRiver/outMSRiver.RData")
load("~/Projects/Current/Westbrook/Brook Trout/Data/Input Data/outMSRiver.RData")



# minimum and maximum sizes 
minsize= 30; 
maxsize=260; 
L= minsize; 
U= maxsize;
          #Do not change. This defines the number of sub-"adult" stages, not yet setup to accomodate more
n <- n.big.matrix <- 100   #size of demographic matrix



# boundary points b and mesh points y
b = L+c(0:n)*(U-L)/n; 
y = 0.5*(b[1:n]+b[2:(n+1)]);

# step size for midpoint rule
h = y[2]-y[1]






p <- adply(invlogit(out$pBeta),.margins= c(1,2,3),mean)
names(p) <- c('season','year2','river2','p')
p <- p[p$river2 != 1,] ; p$river2 <- as.numeric(as.character(p$river2)) - 1
p$river <- 'WEST BROOK'
p$river[p$river2 == 2] <- 'WB JIMMY'
p$river[p$river2 == 3] <- 'WB MITCHELL'
p$river[p$river2 == 4] <- 'WB OBEAR'

p$year <- as.numeric(as.character(p$year2)) + d$minYear - 1
p$season <- as.numeric(as.character(p$season))
p$riverOrdered <- factor(p$river,levels=c('WEST BROOK','WB JIMMY','WB MITCHELL','WB OBEAR'), ordered=T)
####################################################


####################################################
# create data frame of counts
dMData$riverOrdered <- factor(dMData$river,levels=c('WEST BROOK','WB JIMMY','WB MITCHELL','WB OBEAR'), ordered=T)
dMData$numEggs  <- ((0.00187 * dMData$length ^ 2.19) )# * plogis(dMData$length, 75, 1))#*.8
dMData$numEggs[which(dMData$numEggs%in%NA)] <- 0  
  
#YOYAdBoundary <- 80 # make year-specific from dMData?

#YOY <- dMData[which(dMData$age==0),]

#countYOY <- melt( ftable(dMData[dMData$enc == 1 & dMData$length <  YOYAdBoundary ,c('season','year','riverOrdered')]) )
countYOY <- melt( ftable(dMData[dMData$enc == 1 & dMData$age ==  0 ,c('season','year','riverOrdered')]) )
countAd <-  melt( ftable(dMData[dMData$enc == 1 & dMData$age != 0 ,c('season','year','riverOrdered')]) )

#sumEggs <- ddply(dMData[dMData$enc == 1 & dMData$length >= YOYAdBoundary,c('season','year','riverOrdered','numEggs')], c('season','year','riverOrdered'), summarise,sumEggs=sum(numEggs) )
sumEggs <- ddply(dMData[dMData$enc == 1 & dMData$age != 0,c('season','year','riverOrdered','numEggs')], c('season','year','riverOrdered'), summarise,sumEggs=sum(numEggs) )



sumEggs$sumEggs <- sumEggs$sumEggs * 0.5 #females

count <- merge(x=countAd, y=countYOY, by=c('value.season','value.year','value.riverOrdered'), all.x=T)
names(count ) <- c('season','year','river','countAd','countYOY')

count <- merge(x=count, y=sumEggs, by.x=c('season','year','river'),by.y=c('season','year','riverOrdered'), all.x=T)

count$year <- as.numeric(as.character(count$year))
count$season <- as.numeric(as.character(count$season))
count$riverOrdered <- factor(count$river,levels=c('WEST BROOK','WB JIMMY','WB MITCHELL','WB OBEAR'), ordered=T)



####################################################
# merge in p data
countP <- merge( x=count, y=p[,c('season','year','riverOrdered','p')], by=c('season','year','riverOrdered'), all.x=T)
# countP$pAd <- countP$p
# countP <- merge( x=countP, y=p[,c('season','year','riverOrdered','p')], 
#                  by.x=c('season','yearYOY','riverOrdered'),
#                  by.y=c('season','year','riverOrdered'),all.x=T)
# 
# countP$pYOY <- countP$p.y
# countP$p.x <- NULL ; countP$p.y <- NULL
# countP <- countP[order(countP$river,countP$year),]


countP$countYOY[which(countP$countYOY==0)] <- NA #makes the zero a very small number

countP$countAdAdj <- countP$countAd/countP$p

countP$countYOYAdj <- countP$countYOY/countP$p

countP$sumEggsAdj <- countP$sumEggs/countP$p





count2 <- countP[countP$season == 3 & countP$year>2002,]



count2 <- count2[order(count2$river,count2$year),]

count2$YOYLag <- c(count2$countYOY[2:nrow(count2)],NA)
count2$YOYLagAdj <- c(count2$countYOYAdj[2:nrow(count2)],NA)
count2$yearYOY <- count2$year+1


####################################################
# merge in env data




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

###Use this once it has been replaced in the callMSRiver file
# env <- cbind(melt(d$flowForN), melt(d$tempForN))
# env <- env[,-(5:7)]
# names(env) <- c('season','river2','year2','stdF','stdT')
# 
# env$yearAd <- env$year2 + d$minYear - 1
# env$yearYOY <- env$yearAd + 1
# 
# env <- env[env$river2 != 1,] ; env$river2 <- as.numeric(as.character(env$river2)) - 1
# env$river <- 'WEST BROOK'
# env$river[env$river2 == 2] <- 'WB JIMMY'
# env$river[env$river2 == 3] <- 'WB MITCHELL'
# env$river[env$river2 == 4] <- 'WB OBEAR'
# 
# env$riverOrdered <- factor(env$river,levels=c('WEST BROOK','WB JIMMY','WB MITCHELL','WB OBEAR'), ordered=T)
# 
# env <- env[order(env$riverOrdered,env$yearYOY,env$season),]
# 
# env$stdFLag1 <- c(NA,env$stdF[1:(nrow(env)-1)])
# env$stdFLag2 <- c(NA,NA,env$stdF[1:(nrow(env)-2)])
# env$stdFLag3 <- c(NA,NA,NA,env$stdF[1:(nrow(env)-3)])
# env$stdFLag4 <- c(NA,NA,NA,NA,env$stdF[1:(nrow(env)-4)])
# env$stdTLag1 <- c(NA,env$stdT[1:(nrow(env)-1)])
# env$stdTLag2 <- c(NA,NA,env$stdT[1:(nrow(env)-2)])
# env$stdTLag3 <- c(NA,NA,NA,env$stdT[1:(nrow(env)-3)])
# env$stdTLag4 <- c(NA,NA,NA,NA,env$stdT[1:(nrow(env)-4)])

countPE <- merge(x=count2, y=env[,c('season','yearYOY','year','riverOrdered','stdF','stdT','stdFLag1','stdTLag1','stdFLag2','stdTLag2','stdFLag3','stdTLag3','stdFLag4','stdTLag4')], 
                 by.x=c('season','yearYOY','riverOrdered'), by.y=c('season','year','riverOrdered'),all.x=T)

countPE <- countPE[-((dim(countPE)[1]-3):(dim(countPE)[1])),] #THis removes the last year


countPE <- countPE[-c(33:36),]

  
 
########################################
# run a glm on the data
countPE$stdFLag12 <- countPE$stdFLag1^2
countPE$stdFLag22 <- countPE$stdFLag2^2
countPE$stdFLag32 <- countPE$stdFLag3^2
countPE$stdFLag42 <- countPE$stdFLag4^2
countPE$stdTLag12 <- countPE$stdTLag1^2
countPE$stdTLag22 <- countPE$stdTLag2^2
countPE$stdTLag32 <- countPE$stdTLag3^2
countPE$stdTLag42 <- countPE$stdTLag4^2
countPE$survJuv <- countPE$YOYLagAdj/countPE$sumEggsAdj
countPE$survJuv[which(countPE$survJuv%in%NA)] <- 0.012359118
countPE$logitsurvJuv <- log(countPE$survJuv/(1-countPE$survJuv))
countPE$arsinsqrt  <- asin(sqrt(countPE$survJuv))
countPE$yearYOY <- as.factor(countPE$yearYOY)


# countPE$survJuv[29] <- NA
# countPE$logitsurvJuv[29] <- NA

mod <- glm( (logitsurvJuv) ~ 
  
       
   
                                                  
                                        0 + river *     (stdFLag1 + 
                                                  stdTLag1 + 
                                                  stdFLag2 + 
                                                  stdTLag2 +
                                               
                                              
                                                 stdFLag1 * 
                                                 stdTLag1  +
                                                         stdFLag2 * 
                                                           stdTLag2 
                                                      
                                                ) -
                                                  stdFLag1 - 
                                                  stdTLag1 - 
                                                  stdFLag2 - 
                                                  stdTLag2 -
            stdFLag1 * 
              stdTLag1  -
              stdFLag2 * 
              stdTLag2


            
             , family=gaussian,
             data=countPE)#[countPE$river %in% 'WEST BROOK',]  )
summary(mod)

plot(invlogit(mod$fitted.values),invlogit(mod$data$logitsurvJuv))

plot(mod)
 plot(mod$residuals,countPE$stdTLag1)

ggplot(mod, aes(x=(fitted.values),y=(stdTLag1))) +
  geom_line(size=2)+
  geom_text(aes(label=yearYOY)) +
   facet_wrap(~river,scales='free')

ggplot(countPE, aes(x=(year),y=(countAdAdj))) +
  geom_line(size=2)+
  geom_text(aes(label=year)) +
  facet_wrap(~river,scales='free')

ggplot(countPE, aes(x=(year),y=(countAdAdj+countYOYAdj))) +
  geom_line(size=2)+
  geom_text(aes(label=year)) +
  facet_wrap(~river,scales='free')

ggplot(countPE, aes(x=(stdFLag4),y=(stdTLag4))) +
  geom_point(size=2)+
  geom_text(aes(label=yearYOY)) +
  facet_wrap(~river,scales='free') +
  geom_smooth(method = "lm", formula = y ~ poly(x,2), colour = "red")  

#outSj <- mod$coefficients
outSj <- array(NA,c(4,5))
for (r in 1:4){
    
    outSj[r,] <- mod$coefficients[c(1,5,9,13,17,21,25)+(r-1)]
    
}







# need to check that all the years line up and that no years are missing


sJ <- abind(countPE$yearYOY,countPE$river,countPE$survJuv,along=2)
sJ <- sJ[-c(33:36),]
sJriver <- array(0,c(4,8))
for (river in 1:4){
  
    sJriver[river,] <- sJ[which(sJ[,2]==river),3]

}


save.image("~/Projects/Current/Westbrook/Brook Trout/Data/Input Data/outSjSurv.RData")




