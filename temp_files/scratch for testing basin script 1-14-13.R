lat=42.42, long=-72.63
lat=42.491991,long=-72.639097
lat<- 42.415156
long<- -72.627568


lat<-42.415948
long<- -72.635729

lat<-42.415505
long<- -72.623755

rm(list=ls())
lat<-42.427036
long<- -72.660877

testingargs<-list(lat=lat,long=long,basindir="C:/ALR/GitHub/testing/basins",datadir="C:/ALR/GitHub/testing/data")
testingargs
setwd("C:/ALR/GitHub/testing/r")
source("delineate_basin.R")
