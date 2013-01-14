dimnames(popSize) <- list(c("Main","Obear"),c(2003:(2003+years)),c("Total","Recruits","Adults"))

##This is for the westbrook metapopulation and the isolated
v2 <- melt(popSize)
names(v2) <- c("Population","Year","Age","N")
v2$Population <- factor(v2$Population,levels=c("Main","Obear"),ordered=T)
v2$Age <- factor(v2$Age,levels=c("Total","Recruits","Adults"),ordered=T)
#   win.graph(); par(mfrow=c(1,1));
p <- ggplot( v2, aes(Year,N,colour = Age) ) 
pPredPop <- p + facet_wrap(~Population,scales="free") + 
  geom_point() +  theme_bw() + geom_line()+
  scale_x_continuous('Year') +
  scale_y_continuous('N')




##This breaks up the Westbrook into each sub population
NDistMainsum <- array(0,c(3,years+1,3))
for (river in 1:3){
  for (yr in 1:(years+1)){
    for (age in 1:3){
      NDistMainsum[river,yr,age] <- sum(NDistMain[yr,age,(river*(length(y)+1)-length(y)):(river*(length(y)+1))])
    }
  }
}


dimnames(NDistMainsum) <- list(c("Westbrook","Jimmy","Mitchell"),c(2003:(2003+years)),c("Total","Recruits","Adults"))

v2 <- melt(NDistMainsum)
names(v2) <- c("Population","Year","Age","N")
v2$Population <- factor(v2$Population,levels=c("Westbrook","Jimmy","Mitchell"),ordered=T)
v2$Age <- factor(v2$Age,levels=c("Total","Recruits","Adults"),ordered=T)
#   win.graph(); par(mfrow=c(1,1));
p <- ggplot( v2, aes(Year,N,colour = Age) ) 
pPredMain <- p + facet_wrap(~Population,scales="free") + 
  geom_point() +  theme_bw() + geom_line()+
  scale_x_continuous('Year') +
  scale_y_continuous('N')


setwd(directory)
# short.dir <- gsub("./","",directory)
# dir.fish <- paste("/home/austin/WebStuff/Images/",short.dir,"/Stoc_Fish.png",sep="")

png("Stoc_Fish.png",width=1225, height=575, bg="white")
par(mfrow=c(1,1));
sidebysideplot <- grid.arrange(pPredPop,pPredMain,nrow=2) 
dev.off()




