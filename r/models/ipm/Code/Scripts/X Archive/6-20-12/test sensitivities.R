




KSensSeason[2,320,320]0.01138068
pDemoSensSeason[2,2,320,320]0.0004332489
pDemoSensSeason[3,2,320,320]2.299281e-06
pDemoSensSeason[5,2,320,320]4.282153e-05
pDemoSensSeason[6,2,320,320]0.001240914

pDemoSensSeason[1,2,350,350] -4.197898e-06
pDemoSensSeason[4,2,320,320] 2.390278e-05

ylong <- c(1:400)
win.graph(); par(mfrow=c(1,1));
persp(ylong,ylong,KMSens[2,,], theta=60, zlim=c(0,.1), ticktype ="detailed",
      xlab=" Seasonal Temperature", ylab=" Seasonal Flow",zlab="Lambda");title("Season 1");



valuesmetaKMyear2 <- valuesmetaKMyear[1]


#test metaK
a <- metaK[2,320,320]


metaK[2,320,320] <- metaK[2,320,320]+metaK[2,320,320]*.0000001



##MetaM is the block diagonal matrix with migration. Movement probabilities for Stages in each block on the diagonal. 
#Within each block is the probabilities for stage i to move from river j to k  
metaM <- array(0,c(4,length(y)*nRiver,length(y)*nRiver)) 
for (season in 1:4){
  metaM[ season,, ] <- diag(1,length(y)) %x% psi[ season,, ]   #this will have to change if psi is size-specific
}


##loop through different starting seasons to get yearly KMyear matrices with different starting seasons
metaKMyear <- array(0,c(4,length(y)*nRiver,length(y)*nRiver))

for (season in 1:4){                         # firstSeason is the season in question and is the last season in the periodic matrix
  f <- c( season, season : (season+2) %% 4 + 1 )
  metaKMyear[season,,] <-  t(perm) %*% metaM[f[4],,] %*% perm %*% metaK[f[4],,] %*%
    t(perm) %*% metaM[f[3],,] %*% perm %*% metaK[f[3],,] %*%
    t(perm) %*% metaM[f[2],,] %*% perm %*% metaK[f[2],,] %*%
    t(perm) %*% metaM[f[1],,] %*% perm %*% metaK[f[1],,]
  
}

source("Scripts/EigCalcAnalytical.R")

(valuesmetaKMyear[1]-valuesmetaKMyear2)/(metaK[2,320,320]-a)




a <-Sx[20,20,2,4]

Sx[20,20,2,4] <- Sx[20,20,2,4]+Sx[20,20,2,4]*.00000001

source("Scripts/MetaKM.R")
source("Scripts/EigCalcAnalytical.R")
(valuesmetaKMyear[1]-valuesmetaKMyear2)/(Sx[20,20,2,4]-a)

Sx[20,20,2,4] <- a


a <-F1x[20,20,2,4]

F1x[20,20,2,4] <- F1x[20,20,2,4]+F1x[20,20,2,4]*.00000001

source("Scripts/MetaKM.R")
source("Scripts/EigCalcAnalytical.R")
(valuesmetaKMyear[1]-valuesmetaKMyear2)/(F1x[20,20,2,4]-a)

F1x[20,20,2,4] <- a


a <-F3x[20,20,2,4]

F3x[20,20,2,4] <- F3x[20,20,2,4]+F3x[20,20,2,4]*.00000001

source("Scripts/MetaKM.R")
source("Scripts/EigCalcAnalytical.R")
(valuesmetaKMyear[1]-valuesmetaKMyear2)/(F3x[20,20,2,4]-a)

F3x[20,20,2,4] <- a


a <-SJx[20,20,2,4]

SJx[20,20,2,4] <- SJx[20,20,2,4]+SJx[20,20,2,4]*.00000001

source("Scripts/MetaKM.R")
source("Scripts/EigCalcAnalytical.R")
(valuesmetaKMyear[1]-valuesmetaKMyear2)/(SJx[20,20,2,4]-a)

SJx[20,20,2,4] <- a



flow <- 0

sum(KSensSeason[2,,] * metaPdKyxVr[2,2,,] * metaPdDemoFlow[2,2,,] 
+ KSensSeason[2,,] * metaPdKyxVr[2,2,,] * metaPdDemotemp[2,2,,] 
+ KSensSeason[2,,] * metaPdKyxVr[2,2,,] * metaPdDemoFxT[2,2,,])0.0513027



 firstSeason <- 2
 flow <- .000000000000001
 temp <- 0
source("Scripts/Demographic Functions.R")
source("Scripts/ConstCompMatrices.R")
source("Scripts/MetaKM.R")
source("Scripts/EigCalcAnalytical.R")

(valuesmetaKMyear[1]-valuesmetaKMyear2)/(.000000000000001)
