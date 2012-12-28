

###Need to change the sensitivity of f2 and g to have a trade-off



ylong <- abind(0,y,along=1)
######Graphs of Kyx###########
win.graph(); par(mfrow=c(1,1));
persp(y[10:100],y[10:100],Kyx[10:100,10:100], theta=60, zlim=c(0,1),
      xlab="size at time t+1", ylab="size at time t",zlab="Kernel (K(y,x))"); title("Kernel (K(y,x))");

win.graph(); par(mfrow=c(1,1));
persp(ylong,ylong,K[,,2,1], theta=60, zlim=c(0,1),
      xlab="size at time t+1", ylab="size at time t"); title("Kernel (k(y,x))");

win.graph(); par(mfrow=c(1,1));
persp(ylong,ylong,K[,,3,1], theta=60, zlim=c(0,.5),
      xlab="size at time t+1", ylab="size at time t"); title("Kernel (k(y,x))");

win.graph(); par(mfrow=c(1,1));
persp(ylong,ylong,K[,,4,1], theta=60, zlim=c(0,.5),
      xlab="size at time t+1", ylab="size at time t"); title("Kernel (k(y,x))");

win.graph(); par(mfrow=c(1,1));
persp(ylong,ylong,K[,,4,1]%*%K[,,3,1]%*%K[,,2,1]%*%K[,,1,1], theta=60, zlim=c(0,7),
      xlab="size at time t+1", ylab="size at time t"); title("Kernel (k(y,x))");



eigen(K[,,4,1]%*%K[,,3,1]%*%K[,,2,1]%*%K[,,1,1])$values






vecs <- abind(brookAnalysis$vectorsMain,brookAnalysis$vectorsIso,along=3)
v <- array(NA,c(2,4,riverType,(length(y)+1)))
dimnames(v) <- list(c("Stable Size","Reproductive Value"),c("Spring","Summer","Fall","Winter"),c("Westbrook","Jimmy","Mitchell","Obear"),c(ylong))
for (val in 1:2){
  for (season in 1:4){
    for (river in 1:riverType){
      v[val,season,river,] <- vecs[val,season,((river*101-100):(river*101))]
    }
  }
}




ylong <- abind(0,y,along=1)


vitals <- array(0,c(6,4,(length(y)+1)*nRiver ))
for (r in 1:rate){
  for (season in 1:4){
    vitals[r,season,] <- colSums(brookAnalysis$vitalSensMain[r,season,,])
  }
}

sumvitals <- array(0,c(6,4))
for (r in 1:rate){
  for (season in 1:4){
    sumvitals[r,season] <- sum(brookAnalysis$vitalSensMain[r,season,,])
  }
}

dimnames(vitals) <- list(c("Growth","Survival","Fecund","RecruitSize","ProbRepro","OffSurvival"),c("Spring","Summer","Fall","Winter"),c(ylong))
# reproductive value
v2 <- melt(vitals)


names(v2) <- c("Rate","Season","Length","value")
v2$Rate <- factor(v2$Rate,levels=c("Growth","Survival","Fecund","RecruitSize","ProbRepro","OffSurvival"),ordered=T)
v2$Season <- factor(v2$Season,levels=c("Spring","Summer","Fall","Winter"),ordered=T)

win.graph(); par(mfrow=c(1,1));
p <- ggplot( v2, aes(Length,value) ) 
p + facet_grid(Rate~Season,scales="free") + 
  geom_point(aes(colour = Rate)) +  theme_bw() + 
  scale_x_continuous('Body size (mm)')+
  scale_y_continuous('Sensitivity')          
                           




win.graph(); par(mfrow=c(1,1))

metaLambdasAllIters2 <- metaLambdasAllIters[1,]

m <- melt(metaLambdasAllIters2)
names(m) <- c("Lambda")

l <- ggplot(m, aes(x=Lambda)) 
l + geom_density() + theme_bw() +
  scale_x_continuous('Lambda')+
  scale_y_continuous('Frequency')


#######Single Pop sens lamb to flow and temp

envsens[env,r,season,flowpert,temppert]


names(v2) <- c("Rate","Season","Length","value")
v2$Rate <- factor(v2$Rate,levels=c("Growth","Survival","NumOff","OffSize","ProbRepro","OffSurvival"),ordered=T)
v2$Season <- factor(v2$Season,levels=c("Spring","Summer","Fall","Winter"),ordered=T)

win.graph(); par(mfrow=c(1,1));
p <- ggplot( v2, aes(Length,value) ) 
p + facet_wrap(~Season,scales="free") + 
  geom_point(aes(colour = Rate)) +  theme_bw() + 
  scale_x_continuous('Body size (mm)')+
  scale_y_continuous('Sensitivity')




#######Single POP lambda surface

metaLambdas2 <- metaLambdas[1,,,]
#metaLambdas2[metaLambdas2<0.8] <- 0.8


dimnames(metaLambdas2) <- list(c("Spring","Summer","Fall","Winter"),fvariables,tvariables)

metaLambdasdf <- melt(metaLambdas2)
names(metaLambdasdf) <- c("Season","StandardizedFlow","StandardizedTemperature","Lambda")

metaLambdasdf <- melt(metaLambdas2)

names(metaLambdasdf) <- c("Season","StandardizedFlow","StandardizedTemperature","Lambda")
metaLambdasdf$Season <- factor(metaLambdasdf$Season,levels=c("Spring","Summer","Fall","Winter"),ordered=T)

win.graph(); par(mfrow=c(1,1));
v <- ggplot(metaLambdasdf, aes(StandardizedFlow, StandardizedTemperature, z=Lambda))
c <-  v  + facet_wrap(~Season,scales="free") +  geom_tile(aes(fill=Lambda)) + scale_fill_gradient(low="black", high="white") + 
  stat_contour(binwidth=0.05,size=0.5) +  stat_contour(binwidth=0.5,size=1)+
  scale_x_continuous('Standardized Flow')+
  scale_y_continuous('Standardized Temperature') 



metaLambdastemp <- metaLambdas2[,c(which(fvariables==-1),which(fvariables==0),which(fvariables==1)),]


metaLambdasdf <- melt(metaLambdastemp)
names(metaLambdasdf) <- c("Season","StandardizedFlow","StandardizedTemperature","Lambda")
metaLambdasdf$Season <- factor(metaLambdasdf$Season,levels=c("Spring","Summer","Fall","Winter"),ordered=T)
metaLambdasdf$StandardizedFlow <- factor(metaLambdasdf$StandardizedFlow,levels=c("-1","0","1"),ordered=T)
win.graph(); par(mfrow=c(1,1));

p <- ggplot( metaLambdasdf, aes(StandardizedTemperature,Lambda) ) 
l <- p + facet_grid(Season~.) + 
  geom_line(aes(linetype = StandardizedFlow)) +  
  scale_x_continuous('Standardized Temperature')+
  scale_y_continuous('Lambda')           


win.graph(); par(mfrow=c(1,1));
sidebysideplot <- grid.arrange(c,l,ncol=2)



#######Single POP lambda surface no winter

metaLambdas2 <- metaLambdas[1,1:3,,]
#metaLambdas2[metaLambdas2<0.8] <- 0.8


dimnames(metaLambdas2) <- list(c("Spring","Summer","Fall"),fvariables,tvariables)

metaLambdasdf <- melt(metaLambdas2)
names(metaLambdasdf) <- c("Season","StandardizedFlow","StandardizedTemperature","Lambda")

metaLambdasdf <- melt(metaLambdas2)

names(metaLambdasdf) <- c("Season","StandardizedFlow","StandardizedTemperature","Lambda")
metaLambdasdf$Season <- factor(metaLambdasdf$Season,levels=c("Spring","Summer","Fall"),ordered=T)

win.graph(); par(mfrow=c(1,1));
v <- ggplot(metaLambdasdf, aes(StandardizedFlow, StandardizedTemperature, z=Lambda))
c <-  v  + facet_grid(Season~.,scales="free") +  geom_tile(aes(fill=Lambda)) + scale_fill_gradient(low="black", high="white") + 
  stat_contour(binwidth=0.005,size=0.5) +  stat_contour(binwidth=0.5,size=1)+
  scale_x_continuous('Standardized Flow')+
  scale_y_continuous('Standardized Temperature') 



metaLambdastemp <- metaLambdas2[,c(which(fvariables==-1),which(fvariables==0),which(fvariables==1)),]


metaLambdasdf <- melt(metaLambdastemp)
names(metaLambdasdf) <- c("Season","StandardizedFlow","StandardizedTemperature","Lambda")
metaLambdasdf$Season <- factor(metaLambdasdf$Season,levels=c("Spring","Summer","Fall","Winter"),ordered=T)
metaLambdasdf$StandardizedFlow <- factor(metaLambdasdf$StandardizedFlow,levels=c("-1","0","1"),ordered=T)
win.graph(); par(mfrow=c(1,1));

p <- ggplot( metaLambdasdf, aes(StandardizedTemperature,Lambda) ) 
l <- p + facet_grid(Season~.) + 
  geom_line(aes(linetype = StandardizedFlow)) +  
  scale_x_continuous('Standardized Temperature')+
  scale_y_continuous('Lambda')           


win.graph(); par(mfrow=c(1,1));
sidebysideplot <- grid.arrange(c,l,ncol=2)


















metaLambdas2 <- metaLambdas[1,1,,,]
#metaLambdas2[metaLambdas2<0.8] <- 0.8


dimnames(metaLambdas2) <- list(c("Spring","Summer","Fall","Winter"),fvariables,tvariables)

metaLambdasdf <- melt(metaLambdas2)
names(metaLambdasdf) <- c("Season","StandardizedFlow","StandardizedTemperature","Lambda")




metaLambdasdf <- melt(metaLambdas2)

names(metaLambdasdf) <- c("Season","StandardizedFlow","StandardizedTemperature","Lambda")
metaLambdasdf$Season <- factor(metaLambdasdf$Season,levels=c("Spring","Summer","Fall","Winter"),ordered=T)

win.graph(); par(mfrow=c(1,1));
v <- ggplot(metaLambdasdf, aes(StandardizedFlow, StandardizedTemperature, z=Lambda))
c <-  v  + facet_wrap(~Season,scales="free") +  geom_tile(aes(fill=Lambda)) + scale_fill_gradient(low="black", high="white") + 
  stat_contour(binwidth=0.05,size=0.5) +  stat_contour(binwidth=0.5,size=1)+
  scale_x_continuous('Standardized Flow')+
  scale_y_continuous('Standardized Temperature') 



metaLambdastemp <- metaLambdas2[,c(which(fvariables==-1),which(fvariables==0),which(fvariables==1)),]


metaLambdasdf <- melt(metaLambdastemp)
names(metaLambdasdf) <- c("Season","StandardizedFlow","StandardizedTemperature","Lambda")
metaLambdasdf$Season <- factor(metaLambdasdf$Season,levels=c("Spring","Summer","Fall","Winter"),ordered=T)
metaLambdasdf$StandardizedFlow <- factor(metaLambdasdf$StandardizedFlow,levels=c("-1","0","1"),ordered=T)
win.graph(); par(mfrow=c(1,1));

p <- ggplot( metaLambdasdf, aes(StandardizedTemperature,Lambda) ) 
l <- p + facet_grid(Season~.) + 
  geom_line(aes(linetype = StandardizedFlow)) +  
  scale_x_continuous('Standardized Temperature')+
  scale_y_continuous('Lambda')           


win.graph(); par(mfrow=c(1,1));
sidebysideplot <- grid.arrange(c,l,ncol=2)





###MetaLambdas all seasons

metaLambdas2 <- metaLambdas[1,,,]
#metaLambdas2[metaLambdas2<0.8] <- 0.8

dimnames(metaLambdas2) <- list(c("Spring","Summer","Fall","Winter"),fvariables,tvariables)


metaLambdasdf <- melt(metaLambdas2)
names(metaLambdasdf) <- c("Season","StandardizedFlow","StandardizedTemperature","Lambda")
metaLambdasdf$Season <- factor(metaLambdasdf$Season,levels=c("Spring","Summer","Fall","Winter"),ordered=T)

#win.graph(); par(mfrow=c(1,1));
v <- ggplot(metaLambdasdf, aes(StandardizedFlow, StandardizedTemperature, z=Lambda))
c <- v  + facet_wrap(~Season) +  geom_tile(aes(fill=Lambda)) + scale_fill_gradient(low="black", high="white") +  
  stat_contour(binwidth=0.05,size=0.5) +  stat_contour(binwidth=0.5,size=1) +
  scale_x_continuous('Standardized Flow')+
  scale_y_continuous('Standardized Temperature')



metaLambdastemp <- metaLambdas2[,c(which(fvariables==-1),which(fvariables==0),which(fvariables==1)),]


metaLambdasdf <- melt(metaLambdastemp)
names(metaLambdasdf) <- c("Season","StandardizedFlow","StandardizedTemperature","Lambda")
metaLambdasdf$Season <- factor(metaLambdasdf$Season,levels=c("Spring","Summer","Fall","Winter"),ordered=T)
metaLambdasdf$StandardizedFlow <- factor(metaLambdasdf$StandardizedFlow,levels=c("-1","0","1"),ordered=T)
#win.graph(); par(mfrow=c(1,1));

p <- ggplot( metaLambdasdf, aes(StandardizedTemperature,Lambda) ) 
l <- p + facet_grid(Season~.) + 
  geom_line(aes(linetype = StandardizedFlow)) +  
  scale_x_continuous('Standardized Temperature')+
  scale_y_continuous('Lambda')           


win.graph(); par(mfrow=c(1,1));
sidebysideplot <- grid.arrange(c,l,ncol=2)






###MetaLambdas all seasons NO WINTER

metaLambdas2 <- metaLambdas[1,1:3,,]
#metaLambdas2[metaLambdas2<0.8] <- 0.8

dimnames(metaLambdas2) <- list(c("Spring","Summer","Fall"),fvariables,tvariables)


metaLambdasdf <- melt(metaLambdas2)
names(metaLambdasdf) <- c("Season","StandardizedFlow","StandardizedTemperature","Lambda")
metaLambdasdf$Season <- factor(metaLambdasdf$Season,levels=c("Spring","Summer","Fall"),ordered=T)

#win.graph(); par(mfrow=c(1,1));
v <- ggplot(metaLambdasdf, aes(StandardizedFlow, StandardizedTemperature, z=Lambda))
c <- v  + facet_grid(Season~.) +  geom_tile(aes(fill=Lambda)) + scale_fill_gradient(low="black", high="white") +  
  stat_contour(binwidth=0.007,size=0.5) +  stat_contour(binwidth=0.5,size=1) +
  scale_x_continuous('Standardized Flow')+
  scale_y_continuous('Standardized Temperature')



metaLambdastemp <- metaLambdas2[,c(which(fvariables==-1),which(fvariables==0),which(fvariables==1)),]


metaLambdasdf <- melt(metaLambdastemp)
names(metaLambdasdf) <- c("Season","StandardizedFlow","StandardizedTemperature","Lambda")
metaLambdasdf$Season <- factor(metaLambdasdf$Season,levels=c("Spring","Summer","Fall"),ordered=T)
metaLambdasdf$StandardizedFlow <- factor(metaLambdasdf$StandardizedFlow,levels=c("-1","0","1"),ordered=T)
#win.graph(); par(mfrow=c(1,1));

p <- ggplot( metaLambdasdf, aes(StandardizedTemperature,Lambda) ) 
l <- p + facet_grid(Season~.) + 
  geom_line(aes(linetype = StandardizedFlow)) +  
  scale_x_continuous('Standardized Temperature')+
  scale_y_continuous('Lambda')           


win.graph(); par(mfrow=c(1,1));
sidebysideplot <- grid.arrange(c,l,ncol=2)
