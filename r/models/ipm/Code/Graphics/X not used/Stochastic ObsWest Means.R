dimnames(popSize) <- list(c("Main","Obear"),c(2003:2011))

##This is for the westbrook metapopulation and the isolated
v2 <- melt(popSize)
names(v2) <- c("Population","Year","N")
v2$Population <- factor(v2$Population,levels=c("Main","Obear"),ordered=T)
#  win.graph(); par(mfrow=c(1,1));
p <- ggplot( v2, aes(Year,N) ) 
pPredPop <- p + facet_wrap(~Population,scales="free") + 
  geom_point() +  theme_bw() + geom_line()+
  scale_x_continuous('Year') +
  scale_y_continuous('N')




##This breaks up the Westbrook into each sub population
NDistMainsum <- array(0,c(3,years+1))
for (river in 1:3){
  for (yr in 1:(years+1)){
         NDistMainsum[river,yr] <- sum(NDistMain[yr,(river*(length(y)+1)-length(y)):(river*(length(y)+1))])
    }
}


dimnames(NDistMainsum) <- list(c("Westbrook","Jimmy","Mitchell"),c(2003:2011))

v2 <- melt(NDistMainsum)
names(v2) <- c("Population","Year","N")
v2$Population <- factor(v2$Population,levels=c("Westbrook","Jimmy","Mitchell"),ordered=T)
# win.graph(); par(mfrow=c(1,1));
p <- ggplot( v2, aes(Year,N) ) 
pPredMain <-p + facet_wrap(~Population,scales="free") + 
  geom_point() +  theme_bw() + geom_line()+
  scale_x_continuous('Year') +
  scale_y_continuous('N')


###This is for the observed population sizes
  
  popNobsbyyear <- array(NA,c((years+1),4))
  for (yr in (1:(years+1))){
    for (river in 1:4){
      popNobsbyyear[yr,river] <- (sum(popNobsbyYearSeasonSize[yr,3,river,]/mean(invlogit((probCatch[yr,river,])))))/2
    }
  } 
  popNobsbyyear1 <- popNobsbyyear[,1:3]
  
  dimnames(popNobsbyyear1) <- list(c(2003:2011),c("Westbrook","Jimmy","Mitchell"))
  
  ##This is for the westbrook metapopulation and the isolated
  v2 <- melt(popNobsbyyear1)
  names(v2) <- c("Year","Stream","N")
  v2$Stream <- factor(v2$Stream,levels=c("Westbrook","Jimmy","Mitchell"),ordered=T)
  #    win.graph(); par(mfrow=c(1,1));
  p <- ggplot( v2, aes(Year,N) ) 
  pObsMain <- p + facet_wrap(~Stream,scales="free") + 
    geom_point() +  theme_bw() + geom_line()
  scale_x_continuous('Year') +
    scale_y_continuous('N')
  
  
  
  popNobsbyyear2 <- array(NA,c((years+1),2))
  popNobsbyyear2[,1] <-rowSums(popNobsbyyear[,1:3]) 
  popNobsbyyear2[,2] <-(popNobsbyyear[,4])
  
  dimnames(popNobsbyyear2) <- list(c(2003:2011),c("Westbrook","Obear"))
  
  ##This is for the westbrook metapopulation and the isolated
  v2 <- melt(popNobsbyyear2)
  names(v2) <- c("Year","Stream","N")
  v2$Stream <- factor(v2$Stream,levels=c("Westbrook","Obear"),ordered=T)
  #    win.graph(); par(mfrow=c(1,1));
  p <- ggplot( v2, aes(Year,N) ) 
  pObsPop <-p + facet_wrap(~Stream,scales="free") + 
    geom_point() +  theme_bw() + geom_line() +
    scale_x_continuous('Year') +
    scale_y_continuous('N')
  
  
  obsLambda <- array(NA,c(years,4))
  for (row in 1:years){
    for (r in 1:4){
      obsLambda[row,r] <- popNobsbyyear[(row+1),r]/popNobsbyyear[row,r]
    }
  }
  
  obsstochLambda <- colSums(log(obsLambda))/dim(obsLambda)[1]
  exp(obsstochLambda)
  
  
  
  obsLambda2 <- array(NA,c(years,2))
  for (row in 1:years){
    for (r in 1:2){
      obsLambda2[row,r] <- popNobsbyyear2[(row+1),r]/popNobsbyyear2[row,r]
    }
  }
  
  obsstochLambda2 <- colSums(log(obsLambda2))/dim(obsLambda2)[1]
  exp(obsstochLambda2)
  
  
  win.graph(); par(mfrow=c(1,1));
  sidebysideplot <- grid.arrange(pPredPop,pPredMain,pObsPop,pObsMain,ncol=2) 
  #    sidebysideplot <- grid.arrange(pPredPop,pPredMain,ncol=2)





popNobsbyYearSeasonSized <- array(NA,c((years+1),4,length(y)))
for (yr in (1:(years+1))){
  for (river in 1:4){
    popNobsbyYearSeasonSized[yr,river,] <- (popNobsbyYearSeasonSize[yr,3,river,]/mean(invlogit((probCatch[yr,river,]))))
  }
}



NDistByRiver <- array(0,c(4,years+1,length(ylong)))
for (river in 1:4){
  for (yr in 1:(years+1)){
    if (river<4){NDistByRiver[river,yr,] <- (NDistMain[yr,(river*(length(y)+1)-length(y)):(river*(length(y)+1))])}
    if (river==4){NDistByRiver[river,yr,] <- (NDistIso[1,yr,])}
  }
}

dimnames(NDistByRiver) <- list(c("Westbrook","Jimmy","Mitchell","Obear"),c(2003:2011),ylong)





##Absolute numbers

popDist <- array(0,c(4,years+1,2,length(y)))
for (river in 1:4){
  for (yr in 1:(years+1)){
    for (obs in 1:2){
      if (obs==1){popDist[river,yr,1,] <-popNobsbyYearSeasonSized[yr,river,] }
      if (obs==2){popDist[river,yr,2,] <-NDistByRiver[river,yr,(2:length(ylong))] }
    }
  }
}
dimnames(popDist) <- list(c("Westbrook","Jimmy","Mitchell","Obear"),c(2003:2011),c("Obs","Pred"),y)

popDistdf <- melt(popDist)

names(popDistdf) <- c("Stream","Year","ObsPred","Length","N")

popDistdf$Stream <- factor(popDistdf$Stream,levels=c("Westbrook","Jimmy","Mitchell","Obear"),ordered=T)
popDistdf$Year <- factor(popDistdf$Year,levels=c(2003:2011),ordered=T)
popDistdf$ObsPred <- factor(popDistdf$ObsPred,levels=c("Obs","Pred"),ordered=T)

win.graph(); par(mfrow=c(1,1));

p <- ggplot( popDistdf, aes(Length,N) ) 
p + stat_smooth(aes(fill = factor(ObsPred)))   +  theme_bw() +
  facet_grid(Year~Stream,scales="free") +
  scale_x_continuous('Length') +
  scale_y_continuous('N')


##Relative numbers

popDist <- array(0,c(4,years+1,2,length(y)))
for (river in 1:4){
  for (yr in 1:(years+1)){
    for (obs in 1:2){
      if (obs==1){popDist[river,yr,1,] <-popNobsbyYearSeasonSized[yr,river,]/sum(popNobsbyYearSeasonSized[yr,river,]) }
      if (obs==2){popDist[river,yr,2,] <-NDistByRiver[river,yr,(2:length(ylong))]/sum(NDistByRiver[river,yr,(2:length(ylong))]) }
    }
  }
}
dimnames(popDist) <- list(c("Westbrook","Jimmy","Mitchell","Obear"),c(2003:2011),c("Obs","Pred"),y)

popDistdf <- melt(popDist)

names(popDistdf) <- c("Stream","Year","ObsPred","Length","N")

popDistdf$Stream <- factor(popDistdf$Stream,levels=c("Westbrook","Jimmy","Mitchell","Obear"),ordered=T)
popDistdf$Year <- factor(popDistdf$Year,levels=c(2003:2011),ordered=T)
popDistdf$ObsPred <- factor(popDistdf$ObsPred,levels=c("Obs","Pred"),ordered=T)

win.graph(); par(mfrow=c(1,1));

p <- ggplot( popDistdf, aes(Length,N) ) 
p + stat_smooth(aes(fill = factor(ObsPred)))   +  theme_bw() +
  facet_grid(Year~Stream,scales="free") +
  scale_x_continuous('Length') +
  scale_y_continuous('Frequency')




# p <- ggplot( popDistdf, aes(N) ) 
# p + geom_freqpoly(aes(colour=factor(ObsPred)))   +  theme_bw() +
#    facet_grid(Year~Stream,scales="free") +
#    scale_x_continuous('Length') +
#    scale_y_continuous('Frequency')


