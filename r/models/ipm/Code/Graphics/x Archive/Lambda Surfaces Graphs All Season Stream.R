

 dimnames(metaLambdas) <- list(c("Main","Obear"),fvariables,tvariables)


metaLambdasdf <- melt(metaLambdas)
names(metaLambdasdf) <- c("Population","StandardizedFlow","StandardizedTemperature","Lambda")
metaLambdasdf$Population <- factor(metaLambdasdf$Population,levels=c("Main","Obear"),ordered=T)
win.graph(); par(mfrow=c(1,1));
v <- ggplot(metaLambdasdf, aes(StandardizedFlow, StandardizedTemperature, z=Lambda))
v  + facet_grid(.~Population) +  geom_tile(aes(fill=Lambda)) + scale_fill_gradient(low="black", high="white") + stat_contour()
  scale_x_continuous('Standardized Flow')+
  scale_y_continuous('Standardized Temperature')



 
 metaLambdas2 <- metaLambdas
 metaLambdas2[metaLambdas2<0.8] <- 0.8
 
 dimnames(metaLambdas2) <- list(c("Main","Obear"),fvariables,tvariables)
 
 metaLambdasdf <- melt(metaLambdas2)
 names(metaLambdasdf) <- c("Population","StandardizedFlow","StandardizedTemperature","Lambda")
 metaLambdasdf$Population <- factor(metaLambdasdf$Population,levels=c("Main","Obear"),ordered=T)
 #win.graph(); par(mfrow=c(1,1));
 v <- ggplot(metaLambdasdf, aes(StandardizedFlow, StandardizedTemperature, z=Lambda))
 c <- v  + facet_grid(.~Population) +  geom_tile(aes(fill=Lambda)) + scale_fill_gradient(low="black", high="white") +  
   stat_contour(binwidth=0.05,size=0.5) +  stat_contour(binwidth=0.5,size=1) +
   scale_x_continuous('Standardized Flow')+
   scale_y_continuous('Standardized Temperature')
 
 
 
 metaLambdastemp <- metaLambdas2[,c(which(fvariables==-1),which(fvariables==0),which(fvariables==1)),]
 
 
 metaLambdasdf <- melt(metaLambdastemp)
 names(metaLambdasdf) <- c("Population","StandardizedFlow","StandardizedTemperature","Lambda")
 metaLambdasdf$Population <- factor(metaLambdasdf$Population,levels=c("Main","Obear"),ordered=T)
 metaLambdasdf$StandardizedFlow <- factor(metaLambdasdf$StandardizedFlow,levels=c("-1","0","1"),ordered=T)
 #win.graph(); par(mfrow=c(1,1));
 
 p <- ggplot( metaLambdasdf, aes(StandardizedTemperature,Lambda) ) 
 l <- p + facet_grid(.~Population) + 
   geom_line(aes(linetype = StandardizedFlow)) +  
   scale_x_continuous('Standardized Temperature')+
   scale_y_continuous('Lambda')           
 
 
 win.graph(); par(mfrow=c(1,1));
 sidebysideplot <- grid.arrange(c,l,ncol=2)
 