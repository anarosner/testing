<<<<<<< HEAD
lat=42.42; long=-72.63
=======
basin_dir<-"C:/ALR/GitHub/somepath/basins"
data_dir<-"C:/ALR/GitHub/somepath/data"
basin_id<-"west_brook"

lat=42.42, long=-72.63
>>>>>>> aeb7e17439a82cbc5942e6e47ddc55cd250d5d6b
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

lat<-42.414427
long<--72.628512

testingargs<-list(lat=lat,long=long,basin_dir="C:/Documents/GitHub/somepath/basins",data_dir="C:/Documents/GitHub/somepath/data")
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
setwd("C:/Documents/GitHub/testing/r")
# testingargs<-list(basin_dir="C:/Documents/GitHub/testing/somepath/basins",run_dir="C:/Documents/GitHub/testing/somepath/runs/user1234/run3001",data_dir="C:/Documents/GitHub/testing/somepath/data")
testingargs<-list(basin_dir="C:/Documents/GitHub/somepath/basins",run_dir="C:/Documents/GitHub/testing/somepath2/runs/11",data_dir="C:/Documents/GitHub/somepath/data")
# source("old_versions/weather_generator_basin_changes_only.R")
source("weather_generator.R")
save(FINAL_HISTORIC_MONTH,      FINAL_HISTORIC_MONTH1,     FINAL_HISTORIC_MONTH2,   FINAL_HISTORIC_MONTHLY,    FINAL_HISTORIC_MONTHLY1,   FINAL_HISTORIC_MONTHLY2,  FINAL_STOCHASTIC_DAILY,    FINAL_STOCHASTIC_MONTH,    FINAL_STOCHASTIC_MONTH1,  FINAL_STOCHASTIC_MONTH2,   FINAL_STOCHASTIC_MONTHLY,  FINAL_STOCHASTIC_MONTHLY1, file="C:/ALR/GitHub/testing/temp_files/save_stoch_results_scottnew2.RData")


FLOW FLOW FLOW
rm(list=ls())
setwd("C:/Documents/GitHub/testing/r")
# source("./supporting_scripts/plot_functions.R")
# setwd("C:/ALR/GitHub/testing/r")
testingargs<-list(basin_dir="C:/Documents/GitHub/somepath/basins",run_dir="C:/Documents/GitHub/testing/somepath2/runs/14",data_dir="C:/Documents/GitHub/somepath/data")
# testingargs<-list(basin_dir="C:/ALR/GitHub/somepath/basins",run_dir="C:/ALR/GitHub/testing/somepath2/runs/14",data_dir="C:/ALR/GitHub/somepath/data")
source("StreamFlowModel.R")



setwd("C:/Documents/GitHub/testing/r")
testingargs<-list(basin_dir="C:/Documents/GitHub/somepath/basins",run_dir="C:/Documents/GitHub/testing/somepath2/runs/15",data_dir="C:/Documents/GitHub/somepath/data")
source("StreamTemperatureModel.R")




rm(list=ls())
setwd("C:/ALR/GitHub/testing/r")
testingargs<-list(basin_dir="C:/Documents/GitHub/somepath/basins",run_dir="C:/Documents/GitHub/testing/somepath2/runs/15",data_dir="C:/Documents/GitHub/somepath/data")
# testingargs<-list(basin_dir="C:/ALR/GitHub/somepath/basins",run_dir="C:/ALR/GitHub/testing/somepath2/runs/16",data_dir="C:/ALR/GitHub/somepath/data")
source("WestbrookIPM.R")


toJSON(list(basin_dir="/home/ana/testing/somepath/basins",run_dir="/home/ana/testing/somepath2/runs/16",data_dir="/home/ana/testing/somepath/data"))
toJSON(list(basin_dir="/home/node.js/basins",run_dir="/home/ana/testing/somepath2/runs/16",data_dir="/home/node.js/data"))

