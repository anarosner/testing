
m <- melt(stochLambda)
names(m) <- c("Population","Iteration","Lambda")
m$Population <- factor(m$Population,levels=c("Westbrook","Obear"),ordered=T)



l <- ggplot(m, aes(x=Lambda)) 
stochLambPlot <- l + facet_wrap(~Population,scales="free")+geom_density() + theme_bw() +
  scale_x_continuous('Stochastic Lambda')+
  scale_y_continuous('Frequency')

png(filename="Output/Stochastic/Means with Posterior/Graphics/StochasticLambdawithPosterior.png",width=725, height=575, bg="white")
par(mfrow=c(1,1))
sidebysideplot <- grid.arrange(stochLambPlot)
dev.off()