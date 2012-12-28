dimnames(popSize) <- list(c("Main","Obear"),c(1:nIter),c(2003:2011),c("Total","Recruits","Adults"))

##This is for the westbrook metapopulation and the isolated
v2 <- melt(popSize[,,,1])
names(v2) <- c("Population","Iteration","Year","N")
v2$Population <- factor(v2$Population,levels=c("Main","Obear"),ordered=T)
#  win.graph(); par(mfrow=c(1,1));
p <- ggplot( v2, aes(Year,N, group=Iteration) ) 
pPredPop <- p + facet_wrap(~Population,scales="free") + 
  geom_point() +  theme_bw() + geom_line()+
  scale_x_continuous('Year') +
  scale_y_continuous('N')




##This breaks up the Westbrook into each sub population
NDistMainsum <- array(0,c(3,nIter,years+1,3))
for (river in 1:3){
  for (yr in 1:(years+1)){
    for(iter in 1:nIter){
      for (age in 1:3){
      NDistMainsum[river,iter,yr,age] <- sum(NDistMain[iter,yr,age,(river*(length(y)+1)-length(y)):(river*(length(y)+1))])
      }
    }
  }
}


dimnames(NDistMainsum) <- list(c("Westbrook","Jimmy","Mitchell"),c(1:nIter),c(2003:2011),c("Total","Recruits","Adults"))

v2 <- melt(NDistMainsum[,,,1])
names(v2) <- c("Population","Iteration","Year","N")
v2$Population <- factor(v2$Population,levels=c("Westbrook","Jimmy","Mitchell"),ordered=T)
# win.graph(); par(mfrow=c(1,1));
p <- ggplot( v2, aes(Year,N, group=Iteration) ) 
pPredMain <-p + facet_wrap(~Population,scales="free") + 
  geom_point() +  theme_bw() + geom_line()+
  scale_x_continuous('Year') +
  scale_y_continuous('N')



popNobsbyYearSeasonSizeAge <- array(NA,c((years+1),4,4,3,length(y)))

for (ye in 1:(years+1)){
  for (season in 1:4){
    popNobsbyYearSeasonSizeAge[ye,season,1,2,] <- hist(dMData$length[which(dMData$season==season & dMData$year==(2002+ye) & dMData$river=='WEST BROOK' & dMData$age==0)],breaks=abind(L,y+h/2,along=1),plot=F)$counts
    popNobsbyYearSeasonSizeAge[ye,season,1,3,] <- hist(dMData$length[which(dMData$season==season & dMData$year==(2002+ye) & dMData$river=='WEST BROOK' & dMData$age!=0)],breaks=abind(L,y+h/2,along=1),plot=F)$counts
    
    popNobsbyYearSeasonSizeAge[ye,season,2,2,] <- hist(dMData$length[which(dMData$season==season & dMData$year==(2002+ye) & dMData$river=='WB JIMMY' & dMData$age==0)],breaks=abind(L,y+h/2,along=1),plot=F)$counts
    popNobsbyYearSeasonSizeAge[ye,season,2,3,] <- hist(dMData$length[which(dMData$season==season & dMData$year==(2002+ye) & dMData$river=='WB JIMMY' & dMData$age!=0)],breaks=abind(L,y+h/2,along=1),plot=F)$counts
    
    popNobsbyYearSeasonSizeAge[ye,season,3,2,] <- hist(dMData$length[which(dMData$season==season & dMData$year==(2002+ye) & dMData$river=='WB MITCHELL' & dMData$age==0)],breaks=abind(L,y+h/2,along=1),plot=F)$counts
    popNobsbyYearSeasonSizeAge[ye,season,3,3,] <- hist(dMData$length[which(dMData$season==season & dMData$year==(2002+ye) & dMData$river=='WB MITCHELL' & dMData$age!=0)],breaks=abind(L,y+h/2,along=1),plot=F)$counts
    
    popNobsbyYearSeasonSizeAge[ye,season,4,2,] <- hist(dMData$length[which(dMData$season==season & dMData$year==(2002+ye) & dMData$river=='WB OBEAR' & dMData$age==0)],breaks=abind(L,y+h/2,along=1),plot=F)$counts
    popNobsbyYearSeasonSizeAge[ye,season,4,3,] <- hist(dMData$length[which(dMData$season==season & dMData$year==(2002+ye) & dMData$river=='WB OBEAR' & dMData$age!=0)],breaks=abind(L,y+h/2,along=1),plot=F)$counts
    
    popNobsbyYearSeasonSizeAge[ye,season,1,1,] <- hist(dMData$length[which(dMData$season==season & dMData$year==(2002+ye) & dMData$river=='WEST BROOK')],breaks=abind(L,y+h/2,along=1),plot=F)$counts
    popNobsbyYearSeasonSizeAge[ye,season,2,1,] <- hist(dMData$length[which(dMData$season==season & dMData$year==(2002+ye) & dMData$river=='WB JIMMY')],breaks=abind(L,y+h/2,along=1),plot=F)$counts
    popNobsbyYearSeasonSizeAge[ye,season,3,1,] <- hist(dMData$length[which(dMData$season==season & dMData$year==(2002+ye) & dMData$river=='WB MITCHELL')],breaks=abind(L,y+h/2,along=1),plot=F)$counts
    popNobsbyYearSeasonSizeAge[ye,season,4,1,] <- hist(dMData$length[which(dMData$season==season & dMData$year==(2002+ye) & dMData$river=='WB OBEAR')],breaks=abind(L,y+h/2,along=1),plot=F)$counts
    
  }
}

probCatch <- (out$pBeta[,2:10,2:5,,1])

popNobsbyYearSeasonSizeAgeAdj <- array(NA,c((years+1),4,4,3,length(y)))
for (ye in 1:(years+1)){
  for (r in 1:4){
    for (season in 1:4){
      for (age in 1:3){
        popNobsbyYearSeasonSizeAgeAdj[ye,season,r,age,] <-  (popNobsbyYearSeasonSizeAge[ye,season,r,age,]/mean(invlogit(probCatch[season,ye,r,])))/2 #div by two to get females
        
      }
    }
  }
}

popNobsbyYearSeasonSizeAgeAdjSum <- array(NA,c((years+1),4,4,3))
for (ye in 1:(years+1)){
  for (r in 1:4){
    for (season in 1:4){
      for (age in 1:3){
        popNobsbyYearSeasonSizeAgeAdjSum[ye,season,r,age] <-  sum(popNobsbyYearSeasonSizeAgeAdj[ye,season,r,age,])
      }
    }
  }
}


popNobsbyYearSeasonSizeAgeAdjSum1 <- popNobsbyYearSeasonSizeAgeAdjSum[,3,1:3,]
dimnames(popNobsbyYearSeasonSizeAgeAdjSum1) <- list(c(2003:2011),c("Westbrook","Jimmy","Mitchell"),c("Total","Recruits","Adults"))

##This is for the westbrook 
v2 <- melt(popNobsbyYearSeasonSizeAgeAdjSum1[,,1])
names(v2) <- c("Year","Stream","N")
v2$Stream <- factor(v2$Stream,levels=c("Westbrook","Jimmy","Mitchell"),ordered=T)
#   win.graph(); par(mfrow=c(1,1));
p <- ggplot( v2, aes(Year,N) ) 
pObsMain <- p + facet_wrap(~Stream,scales="free") + 
  geom_point() +  theme_bw() + geom_line()+
  scale_x_continuous('Year') +
  scale_y_continuous('N')



popNobsbyYearSeasonSizeAgeAdjSum2 <- array(NA,c((years+1),2,3))
for (age in 1:3){
  popNobsbyYearSeasonSizeAgeAdjSum2[,1,age] <-rowSums(popNobsbyYearSeasonSizeAgeAdjSum[,3,1:3,age])
}
popNobsbyYearSeasonSizeAgeAdjSum2[,2,] <-(popNobsbyYearSeasonSizeAgeAdjSum[,3,4,])

dimnames(popNobsbyYearSeasonSizeAgeAdjSum2) <- list(c(2003:2011),c("Westbrook","Obear"),c("Total","Recruits","Adults"))

##This is for the westbrook metapopulation and the isolated
v2 <- melt(popNobsbyYearSeasonSizeAgeAdjSum2[,,1])
names(v2) <- c("Year","Stream","N")
v2$Stream <- factor(v2$Stream,levels=c("Westbrook","Obear"),ordered=T)

#   win.graph(); par(mfrow=c(1,1));
p <- ggplot( v2, aes(Year,N) ) 
pObsPop <- p + facet_wrap(~Stream,scales="free") + 
  geom_point() +  theme_bw() + geom_line()+
  scale_x_continuous('Year') +
  scale_y_continuous('N')

png(filename="Output/Stochastic/Means with Posterior/Graphics/ObsvPredTotalwithPosterior.png",width=725, height=575, bg="white")
par(mfrow=c(1,1));
sidebysideplot <- grid.arrange(pPredPop,pPredMain,pObsPop,pObsMain,ncol=2) 
dev.off()
