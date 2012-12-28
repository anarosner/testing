


ylong <- abind(0,y,along=1)

vecs <- (brookAnalysis$vectorsMain)
v <- array(NA,c(2,4,3,(length(y)+1)))
dimnames(v) <- list(c("Stable Size","Reproductive Value"),c("Spring","Summer","Fall","Winter"),c("Main","OL","OS"),c(ylong))
for (val in 1:2){
  for (season in 1:4){
    for (river in 1:3){
      v[val,season,river,] <- vecs[val,season,((river*101-100):(river*101))]
    }
  }
}



# reproductive value
v2 <- melt(v[2,,,])

names(v2) <- c("Season","Stream","Length","RV")
v2$Season <- factor(v2$Season,levels=c("Spring","Summer","Fall","Winter"),ordered=T)
v2$Stream <- factor(v2$Stream,levels=c("Main","OL","OS"),ordered=T)
win.graph(); par(mfrow=c(1,1));
p <- ggplot( v2, aes(Length,RV) ) 
p + facet_wrap(~Season) + 
   geom_point(aes(colour = Stream)) +  theme_bw() +
   scale_x_continuous('Body size (mm)')+
   scale_y_continuous('Reproductive Value')           

# Stable Size with pre-recruits
v1 <- melt(v[1,,,])

names(v1) <- c("Season","Stream","Length","SS")
v1$Season <- factor(v1$Season,levels=c("Spring","Summer","Fall","Winter"),ordered=T)
v1$Stream <- factor(v1$Stream,levels=c("Main","OL","OS"),ordered=T)
win.graph(); par(mfrow=c(1,1));
p <- ggplot( v1, aes(Length,SS) ) 
p + facet_wrap(~Season) + 
  geom_point(aes(colour = Stream)) +  
  scale_x_continuous('Body size (mm)')+
  scale_y_continuous('Frequency')           

# Stable Size without pre-recruits
v1 <- v[1,,,(2:101)]
# for (season in 1:4){
#   for (river in 1:4){
#     v1[season,river,] <- v1[season,river,]/sum(v1[season,river,])
#   }
# }

v1 <- melt(v1)

names(v1) <- c("Season","Stream","Length","SS")
v1$Season <- factor(v1$Season,levels=c("Spring","Summer","Fall","Winter"),ordered=T)
v1$Stream <- factor(v1$Stream,levels=c("Main","OL","OS"),ordered=T)
win.graph(); par(mfrow=c(1,1));
p <- ggplot( v1, aes(Length,SS) ) 
p + facet_wrap(~Season,scales="free") + 
  geom_point(aes(colour = Stream)) +  
  scale_x_continuous('Body size (mm)')+
  scale_y_continuous('Frequency')           

