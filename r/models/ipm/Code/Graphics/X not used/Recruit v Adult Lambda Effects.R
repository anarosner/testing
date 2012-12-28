

popGr <- array(NA,c(npop,years,2))
for (ye in 1:(years)){
  for (pop in 1:2){
    popGr[pop,ye,1] <- popSize[pop,ye+1,2]/popSize[pop,ye,3]
    popGr[pop,ye,2] <- popNobsbyYearSeasonSizeAgeAdjSum2[ye+1,pop,2]/popNobsbyYearSeasonSizeAgeAdjSum2[ye,pop,3]
    }
 }

dimnames(popGr) <- list(c("Westbrook","Obear"),c(2004:2011),c("Pred","Obs"))

v2 <- melt(popGr)

names(v2) <- c("Stream","Year","Source","LambdaContrib")
v2$Stream <- factor(v2$Stream,levels=c("Westbrook","Obear"),ordered=T)
v2$Source <- factor(v2$Source,levels=c("Pred","Obs"),ordered=T)
 #  win.graph(); par(mfrow=c(1,1));

p <- ggplot( v2, aes(Year,LambdaContrib,colour = Source) ) 
popMeta <- p + facet_wrap(~Stream,scales="free") + 
  geom_point() +  theme_bw() + geom_line()+
  scale_x_continuous('Year') +
  scale_y_continuous('LambaContrib')




popGr <- array(NA,c(3,years,2))
for (ye in 1:(years)){
  for (pop in 1:3){
    popGr[pop,ye,1] <- NDistMainsum[pop,ye+1,2]/NDistMainsum[pop,ye,3]
    popGr[pop,ye,2] <- popNobsbyYearSeasonSizeAgeAdjSum1[ye+1,pop,2]/popNobsbyYearSeasonSizeAgeAdjSum1[ye,pop,3]
  }
}

dimnames(popGr) <- list(c("Westbrook","Jimmy","Mitchell"),c(2004:2011),c("Pred","Obs"))

v2 <- melt(popGr)

names(v2) <- c("Stream","Year","Source","LambdaContrib")
v2$Stream <- factor(v2$Stream,levels=c("Westbrook","Jimmy","Mitchell"),ordered=T)
v2$Source <- factor(v2$Source,levels=c("Pred","Obs"),ordered=T)
#  win.graph(); par(mfrow=c(1,1));

p <- ggplot( v2, aes(Year,LambdaContrib,colour = Source) ) 
popWest <- p + facet_wrap(~Stream,scales="free") + 
  geom_point() +  theme_bw() + geom_line()+
  scale_x_continuous('Year') +
  scale_y_continuous('LambaContrib')

win.graph(); par(mfrow=c(1,1));
sidebysideplot <- grid.arrange(popMeta,popWest,ncol=1) 
#    sidebysideplot <- grid.arrange(pPredPop,pPredMain,ncol=2)


















###Adult effects


popGr <- array(NA,c(npop,years,2))
for (ye in 1:(years)){
  for (pop in 1:2){
    popGr[pop,ye,1] <- popSize[pop,ye+1,3]/(popSize[pop,ye,3]+popSize[pop,ye,2])
    popGr[pop,ye,2] <- popNobsbyYearSeasonSizeAgeAdjSum2[ye+1,pop,3]/(popNobsbyYearSeasonSizeAgeAdjSum2[ye,pop,3]+popNobsbyYearSeasonSizeAgeAdjSum2[ye+1,pop,2])
  }
}

dimnames(popGr) <- list(c("Westbrook","Obear"),c(2004:2011),c("Pred","Obs"))

v2 <- melt(popGr)

names(v2) <- c("Stream","Year","Source","LambdaContrib")
v2$Stream <- factor(v2$Stream,levels=c("Westbrook","Obear"),ordered=T)
v2$Source <- factor(v2$Source,levels=c("Pred","Obs"),ordered=T)
#  win.graph(); par(mfrow=c(1,1));

p <- ggplot( v2, aes(Year,LambdaContrib,colour = Source) ) 
popMeta <- p + facet_wrap(~Stream,scales="free") + 
  geom_point() +  theme_bw() + geom_line()+
  scale_x_continuous('Year') +
  scale_y_continuous('LambaContrib')




popGr <- array(NA,c(3,years,2))
for (ye in 1:(years)){
  for (pop in 1:3){
    popGr[pop,ye,1] <- NDistMainsum[pop,ye+1,3]/(NDistMainsum[pop,ye,2]+NDistMainsum[pop,ye,3])
    popGr[pop,ye,2] <- popNobsbyYearSeasonSizeAgeAdjSum1[ye+1,pop,3]/(popNobsbyYearSeasonSizeAgeAdjSum1[ye,pop,2]+popNobsbyYearSeasonSizeAgeAdjSum1[ye,pop,3])
  }
}

dimnames(popGr) <- list(c("Westbrook","Jimmy","Mitchell"),c(2004:2011),c("Pred","Obs"))

v2 <- melt(popGr)

names(v2) <- c("Stream","Year","Source","LambdaContrib")
v2$Stream <- factor(v2$Stream,levels=c("Westbrook","Jimmy","Mitchell"),ordered=T)
v2$Source <- factor(v2$Source,levels=c("Pred","Obs"),ordered=T)
#  win.graph(); par(mfrow=c(1,1));

p <- ggplot( v2, aes(Year,LambdaContrib,colour = Source) ) 
popWest <- p + facet_wrap(~Stream,scales="free") + 
  geom_point() +  theme_bw() + geom_line()+
  scale_x_continuous('Year') +
  scale_y_continuous('LambaContrib')

win.graph(); par(mfrow=c(1,1));
sidebysideplot <- grid.arrange(popMeta,popWest,ncol=1) 
#    sidebysideplot <- grid.arrange(pPredPop,pPredMain,ncol=2)