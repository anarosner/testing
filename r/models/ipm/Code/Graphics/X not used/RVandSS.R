ylong <- abind(0,y,along=1)

vecs <- abind(brookAnalysis$vectorsMain,brookAnalysis$vectorsIso,along=3)
v <- array(NA,c(2,4,riverType,(length(y)+1)))
dimnames(v) <- list(c("Stable Size","Reproductive Value"),c("Spring","Summer","Fall","Winter"),c("Westbrook","Jimmy","Mitchell","Obear"),c(ylong))
for (val in 1:2){
  for (season in 1:4){
    for (river in 1:riverType){
      v[val,season,river,] <- vecs[val,season,((river*(length(y)+1)-length(y)):(river*(length(y)+1)))]
    }
  }
}


# reproductive value
v2 <- melt(v[2,,(1:3),])

names(v2) <- c("Season","Stream","Length","RV")
v2$Season <- factor(v2$Season,levels=c("Spring","Summer","Fall","Winter"),ordered=T)
v2$Stream <- factor(v2$Stream,levels=c("Westbrook","Jimmy","Mitchell"),ordered=T)
p <- ggplot( v2, aes(Length,RV) ) 
main <- p + facet_wrap(~Season,scales='free') + 
  geom_point(aes(colour = Stream)) +  theme_bw() +
  scale_x_continuous('Body size (mm)')+
  scale_y_continuous('Reproductive Value')           


v2 <- melt(v[2,,4,])

names(v2) <- c("Season","Length","RV")
v2$Season <- factor(v2$Season,levels=c("Spring","Summer","Fall","Winter"),ordered=T)
p <- ggplot( v2, aes(Length,RV) ) 
iso <- p + facet_wrap(~Season,scales='free') + 
  geom_point(aes()) +  theme_bw() +
  scale_x_continuous('Body size (mm)')+
  scale_y_continuous('Reproductive Value')



png(filename="Output/Prospective/Means/Graphics/ReproductiveValue.png",width=725, height=575, bg="white")
par(mfrow=c(1,1))
sidebysideplot <- grid.arrange(main,iso,ncol=2)
dev.off()


# Stable Size with pre-recruits
v2 <- melt(v[1,,(1:3),])

names(v2) <- c("Season","Stream","Length","SS")
v2$Season <- factor(v2$Season,levels=c("Spring","Summer","Fall","Winter"),ordered=T)
v2$Stream <- factor(v2$Stream,levels=c("Westbrook","Jimmy","Mitchell"),ordered=T)
p <- ggplot( v2, aes(Length,SS) ) 
main <- p + facet_wrap(~Season,scales='free') + 
  geom_point(aes(colour = Stream)) +  theme_bw() +
  scale_x_continuous('Body size (mm)')+
  scale_y_continuous('Frequency')           


v2 <- melt(v[1,,4,])

names(v2) <- c("Season","Length","SS")
v2$Season <- factor(v2$Season,levels=c("Spring","Summer","Fall","Winter"),ordered=T)
p <- ggplot( v2, aes(Length,SS) ) 
iso <- p + facet_wrap(~Season,scales='free') + 
  geom_point(aes()) +  theme_bw() +
  scale_x_continuous('Body size (mm)')+
  scale_y_continuous('Frequency')


png(filename="Output/Prospective/Means/Graphics/StableSizeWithPreRecruits.png",width=725, height=575, bg="white")
par(mfrow=c(1,1));
sidebysideplot <- grid.arrange(main,iso,ncol=2)
dev.off()


# Stable Size without pre-recruits

v2 <- melt(v[1,,(1:3),(2:(length(y)+1))])

names(v2) <- c("Season","Stream","Length","SS")
v2$Season <- factor(v2$Season,levels=c("Spring","Summer","Fall","Winter"),ordered=T)
v2$Stream <- factor(v2$Stream,levels=c("Westbrook","Jimmy","Mitchell"),ordered=T)
p <- ggplot( v2, aes(Length,SS) ) 
main <- p + facet_wrap(~Season,scales='free') + 
  geom_point(aes(colour = Stream)) +  theme_bw() +
  scale_x_continuous('Body size (mm)')+
  scale_y_continuous('Frequency')           


v2 <- melt(v[1,,4,(2:(length(y)+1))])

names(v2) <- c("Season","Length","SS")
v2$Season <- factor(v2$Season,levels=c("Spring","Summer","Fall","Winter"),ordered=T)
p <- ggplot( v2, aes(Length,SS) ) 
iso <- p + facet_wrap(~Season,scales='free') + 
  geom_point(aes()) +  theme_bw() +
  scale_x_continuous('Body size (mm)')+
  scale_y_continuous('Frequency')


png(filename="Output/Deterministic/Prospective/Means/Graphics/StableSizeNoPreRecruits.png",width=725, height=575, bg="white")
par(mfrow=c(1,1));
sidebysideplot <- grid.arrange(main,iso,ncol=2)
dev.off()