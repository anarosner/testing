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

testingargs<-list(lat=lat,long=long,basin_dir="C:/Documents/GitHub/testing/somepath/basins",data_dir="C:/Documents/GitHub/testing/somepath/data")
testingargs
setwd("C:/Documents/GitHub/testing/r")
source("delineate_basin.R")

rm(list=ls())

setwd("C:/Documents/GitHub/testing/r")
testingargs<-list(basin_dir="C:/Documents/GitHub/testing/somepath/basins",run_dir="C:/Documents/GitHub/testing/somepath/runs/user1234/run9001",data_dir="C:/Documents/GitHub/testing/somepath/data")
source("weather_generator basin changes only.R")



rm(list=ls())
setwd("C:/Documents/GitHub/testing/r")
testingargs<-list(basin_dir="C:/Documents/GitHub/testing/somepath/basins",run_dir="C:/Documents/GitHub/testing/somepath/runs/user1234/run2001",data_dir="C:/Documents/GitHub/testing/somepath/data")
source("weather_generator.R")

rm(list=ls())
# setwd("C:/Documents/GitHub/testing/r")
setwd("C:/ALR/GitHub/testing/r")
# testingargs<-list(basin_dir="C:/Documents/GitHub/testing/somepath/basins",run_dir="C:/Documents/GitHub/testing/somepath/runs/user1234/run3001",data_dir="C:/Documents/GitHub/testing/somepath/data")
testingargs<-list(basin_dir="C:/ALR/GitHub/somepath/basins",run_dir="C:/ALR/GitHub/testing/somepath2/runs/11",data_dir="C:/ALR/GitHub/somepath/data")
source("weather_generator.R")

rm(list=ls())
# setwd("C:/Documents/GitHub/testing/r")
setwd("C:/ALR/GitHub/testing/r")
# testingargs<-list(basin_dir="C:/Documents/GitHub/testing/somepath/basins",run_dir="C:/Documents/GitHub/testing/somepath/runs/user1234/run1004",data_dir="C:/Documents/GitHub/testing/somepath/data")
testingargs<-list(basin_dir="C:/ALR/GitHub/somepath/basins",run_dir="C:/ALR/GitHub/testing/somepath2/runs/12",data_dir="C:/ALR/GitHub/somepath/data")
source("StreamFlowModel.R")

rm(list=ls())
setwd("C:/ALR/GitHub/testing/r")
testingargs<-list(basin_dir="C:/ALR/GitHub/somepath/basins",run_dir="C:/ALR/GitHub/testing/somepath2/runs/16",data_dir="C:/ALR/GitHub/somepath/data")
source("WestbrookIPM.R")


toJSON(list(basin_dir="/home/ana/testing/somepath/basins",run_dir="/home/ana/testing/somepath2/runs/16",data_dir="/home/ana/testing/somepath/data"))
toJSON(list(basin_dir="/home/node.js/basins",run_dir="/home/ana/testing/somepath2/runs/16",data_dir="/home/node.js/data"))

