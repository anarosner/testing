
###For the quasi-extiction threshold
quasi <- popSize[,,,1]
quasi[quasi < quasiextinct] <- 1
quasi[quasi >= quasiextinct] <- 0

for (pop in 1:npop){ 
  for (r in 1:nIter){
    for (col in 1:(years+1)){
      if (col==1){
        quasi[pop,r,col] <- quasi[pop,r,col]
      }
      if (col>1){
        if (quasi[pop,r,col]==0){
          quasi[pop,r,col] <- quasi[pop,r,(col-1)]
        }
        if (quasi[pop,r,col]==1){
          quasi[pop,r,col] <- quasi[pop,r,col]
        }
      }
    }
  }
}

quasiextcum <- array(0,c(npop,(years+1)))
for (pop in 1:npop){
  quasiextcum[pop,] <- colSums(quasi[pop,,])/nIter
}

q <- array(0,c(npop,nIter,(years+1)))
for (pop in 1:npop){
  for (r in 1:nIter){
    for (col in 1:(years+1)){
      q[pop,r,col] <- sum(quasi[pop,r,(1:col)])+q[pop,r,col]
      q[pop,r,col] <- sum(q[pop,r,(1:col)])
    }
  }
  quasi[q>1]=0
}

quasiext <- array(0,c(npop,(years+1)))
for (pop in 1:npop){
  quasiext[pop,] <- colSums(quasi[pop,,])/nIter
}  



v2 <- quasiext
dimnames(v2) <- list(c("Main","Obear"),c(2003:2011))
m <- melt(v2)
names(m) <- c("Population","Year","Quasi")
m$Population <- factor(m$Population,levels=c("Main","Obear"),ordered=T)

l <- ggplot(m, aes(x=Year,y=Quasi)) 
quasiProb <- l + facet_wrap(~Population)+geom_line() + theme_bw() +
  scale_x_continuous('Year')+
  scale_y_continuous('Probability of Quasi-Extinction')

png(filename="Output/Stochastic/Means with Posterior/Graphics/ProbQuasiExtinction.png",width=725, height=575, bg="white")
par(mfrow=c(1,1))
sidebysideplot <- grid.arrange(quasiProb)
dev.off()

v2 <- quasiextcum
dimnames(v2) <- list(c("Main","Obear"),c(2003:2011))
m <- melt(v2)
names(m) <- c("Population","Year","Quasi")
m$Population <- factor(m$Population,levels=c("Main","Obear"),ordered=T)

l <- ggplot(m, aes(x=Year,y=Quasi)) 
cumQuasiProb <- l + facet_wrap(~Population)+geom_line() + theme_bw() +
  scale_x_continuous('Year')+
  scale_y_continuous('Cumulative Probability of Quasi-Extinction')

png(filename="Output/Stochastic/Means with Posterior/Graphics/CumProbQuasiExtinction.png",width=725, height=575, bg="white")
par(mfrow=c(1,1))
sidebysideplot <- grid.arrange(cumQuasiProb)
dev.off()