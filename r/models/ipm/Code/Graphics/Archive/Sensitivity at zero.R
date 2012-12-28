

persp(c(1:102),c(1:102),KSensMain[3,1:102,1:102], theta=60, zlim=c(0,1),
      xlab="size at time t+1", ylab="size at time t"); title("Kernel (k(y,x))");




#KSensMain2[1,4,,,] <- metaLambdas2[2,4,,,]

KSensAll <- array(0,c(4,4,length(y)+1,length(y)+1))
for (season in 1:3){
  for (river in 1:3){
    KSensAll[season,river,,] <- KSensMain[season,(river*(length(y)+1)-length(y)):(river*(length(y)+1)),(river*(length(y)+1)-length(y)):(river*(length(y)+1))]
  }
}
for (season in 1:3){

    KSensAll[season,4,,] <- KSensIso[1,season,,]

}
dimnames(KSensAll) <- list(c("Spring","Summer","Fall","Winter"),c("Westbrook","Jimmy","Mitchell","Obear"))

KSensMaindf <- melt(KSensAll)
names(KSensMaindf) <- c("Season","Stream","lengthy","lengthx","Sens")



KSensMaindf$Season <- factor(KSensMaindf$Season,levels=c("Spring","Summer","Fall","Winter"),ordered=T)
KSensMaindf$Stream <- factor(KSensMaindf$Stream,levels=c("Westbrook","Jimmy","Mitchell","Obear"),ordered=T)

#win.graph(); par(mfrow=c(1,1));
v <- ggplot(KSensMaindf, aes(lengthx, lengthy, z=Sens))
v  + facet_grid(Season~Stream) +  
  geom_tile(aes(fill=Sens)) + 
  scale_fill_gradient(low="white", high="black") + 
  stat_contour(binwidth=0.5,size=0.5) +
  scale_x_continuous('Length t')+
  scale_y_continuous('Length t+1') 



# metaLambdastemp <- metaLambdas2[1,,,c(which(fvariables==-1),which(fvariables==0),which(fvariables==1)),]
# 
# 
# metaLambdasdf <- melt(metaLambdastemp)
# names(metaLambdasdf) <- c("Stream","Season","StandardizedFlow","StandardizedTemperature","Lambda")
# metaLambdasdf$Season <- factor(metaLambdasdf$Season,levels=c("Spring","Summer","Fall","Winter"),ordered=T)
# metaLambdasdf$Stream <- factor(metaLambdasdf$Stream,levels=c("Westbrook","Jimmy","Mitchell","Obear"),ordered=T)
# metaLambdasdf$StandardizedFlow <- factor(metaLambdasdf$StandardizedFlow,levels=c("-1","0","1"),ordered=T)
# #win.graph(); par(mfrow=c(1,1));
# 
# p <- ggplot( metaLambdasdf, aes(StandardizedTemperature,Lambda) ) 
# l <- p + facet_grid(Season~Stream) + 
#   geom_line(aes(linetype = StandardizedFlow)) +  
#   scale_x_continuous('Standardized Temperature')+
#   scale_y_continuous('Lambda')           
# 
# 
# win.graph(); par(mfrow=c(1,1));
# sidebysideplot <- grid.arrange(c,l,ncol=2)
