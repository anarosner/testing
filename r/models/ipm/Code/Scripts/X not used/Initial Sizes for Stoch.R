
# #####To get initial size dists and numbers
load("~/Projects/Current/Westbrook/Brook Trout/Data/dMDataOutBKT2002_2011.RData")

dMData$length[dMData$tagNumberCH=='1BF1FF6207' & dMData$season == 3 & dMData$year == 2005] <- NA
dMData$length[dMData$tagNumberCH=='1BF1FF6521' & dMData$season == 3 & dMData$year == 2005] <- NA
dMData$length[dMData$tagNumberCH=='1BF18CE7ED' & dMData$season == 2 & dMData$year == 2006] <- NA
dMData$length[dMData$tagNumberCH=='1BF20FF1B9' & dMData$season == 3 & dMData$year == 2005] <- NA
dMData$length[dMData$tagNumberCH=='257C67CA48' ] <- NA
dMData$length[dMData$tagNumberCH=='1BF20EB7A4' & dMData$season == 4 & dMData$year == 2008] <- NA

dMData$riverOrdered <- factor(dMData$river,levels=c('WEST BROOK','WB JIMMY','WB MITCHELL','WB OBEAR'), ordered=T)

lengthByRiverSeasonYear <- ddply( dMData, .(riverOrdered,riverN,year,season), summarise,   
                                  length)

y2002 <- lengthByRiverSeasonYear[which(lengthByRiverSeasonYear[,3]==2002),]
y2002sea3 <- y2002[which(y2002$season==3),]
y2002sea3 <- y2002sea3[-298,]
y2002sea3 <- y2002sea3[-298,]
y2002sea3 <- y2002sea3[-298,]
y2002sea3 <- y2002sea3[-298,]
y2002sea3 <- y2002sea3[-298,]
y2002sea3 <- y2002sea3[-1,]
y2002sea3 <- y2002sea3[,-2]
y2002sea3 <- y2002sea3[,-2]
y2002sea3 <- y2002sea3[,-2]

names(y2002sea3) <- c("Population","Length")

win.graph(); par(mfrow=c(1,1))

l <- ggplot(y2002sea3, aes(x=Length)) 
l + facet_wrap(~Population)+geom_density() + theme_bw() +
  scale_x_continuous('Length')+
  scale_y_continuous('Frequency')