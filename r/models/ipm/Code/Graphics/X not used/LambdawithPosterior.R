
dimnames(metaLambdasAllIters) <- list(c("Main","Obear"),c(1:nIter))
m <- melt(metaLambdasAllIters)
names(m) <- c("Population","Iteration","Lambda")
m$Population <- factor(m$Population,levels=c("Main","Obear"),ordered=T)

l <- ggplot(m, aes(x=Lambda)) 
lamPost <- l + facet_wrap(~Population,scales="free")+geom_density() + theme_bw() +
  scale_x_continuous('Lambda')+
  scale_y_continuous('Frequency')

png(filename="Output/Deterministic/Prospective/Means with Posterior/Graphics/LambdawithPosterior.png",width=725, height=575, bg="white")
par(mfrow=c(1,1))
sidebysideplot <- grid.arrange(lamPost)
dev.off()