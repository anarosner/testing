<<<<<<< HEAD
lat=42.42; long=-72.63
=======
basin_dir<-"C:/ALR/GitHub/somepath/basins"
data_dir<-"C:/ALR/GitHub/somepath/data"
basin_id<-"west_brook"
run_base_dir<-"C:/ALR/GitHub/testing/somepath2/runs"


setwd("C:/alr/GitHub/testing/r")
testingargs<-list(basin_dir=basin_dir,data_dir=data_dir, run_dir= paste0(run_base_dir,"/no_change"))
source("weather_generator.R")
setwd("C:/alr/GitHub/testing/r")
testingargs<-list(basin_dir=basin_dir,data_dir=data_dir, run_dir= paste0(run_base_dir,"/no_change_flow"))
source("StreamFlowModel.R")
setwd("C:/alr/GitHub/testing/r")
testingargs<-list(basin_dir=basin_dir,data_dir=data_dir, run_dir= paste0(run_base_dir,"/no_change_streamtemp"))
source("StreamTemperatureModel.R")

toJSON(list(basin_dir="/home/node.js/basins",
            data_dir="/home/node.js/data",
            run_dir="/home/node.js/users/testuser1/climate/93bc1e5e-b6f7-40ea-a4c1-a47c25c44621"))

testingargs<-list(basin_dir="/home/node.js/basins",
            data_dir="/home/node.js/data",
            run_dir="/home/node.js/users/testuser1/climate/93bc1e5e-b6f7-40ea-a4c1-a47c25c44621")
setwd("/home/node.js/ana-testing-repo/r")
testingargs<-list(basin_dir="/home/node.js/basins",
                  data_dir="/home/node.js/data",
                  run_dir="/home/ana")


# toJSON(list(basin_dir="/home/node.js/basins",
#             data_dir="/home/node.js/data",
#             run_dir="/home/ana/testing/somepath2/runs/no_change"))
# toJSON(list(basin_dir="/home/node.js/basins",
#             data_dir="/home/node.js/data",
#             run_dir="/home/ana/testing/somepath2/runs/no_change_flow"))
# toJSON(list(basin_dir="/home/node.js/basins",
#             data_dir="/home/node.js/data",
#             run_dir="/home/ana/testing/somepath2/runs/no_change_streamtemp"))
toJSON(list(basin_dir="/home/node.js/basins",
            data_dir="/home/node.js/data",
            run_dir="/home/ana/testing/somepath2/for_k/drier_fish"))
source("salmon_IPM.R")


for ( j in c("hotter","wetter","drier") ) {
  run_base_dir<-"C:/ALR/GitHub/testing/somepath2/for_k"
  setwd("C:/alr/GitHub/testing/r")
  testingargs<-list(basin_dir=basin_dir,data_dir=data_dir, run_dir= paste0(run_base_dir,"/", j, "_flow"))
  source("StreamFlowModel.R")
  setwd("C:/alr/GitHub/testing/r")
  testingargs<-list(basin_dir=basin_dir,data_dir=data_dir, run_dir= paste0(run_base_dir,"/", j, "_streamtemp"))
  source("StreamTemperatureModel.R")
  # setwd("C:/alr/GitHub/testing/r")
  # testingargs<-list(basin_dir=basin_dir,data_dir=data_dir, run_dir= paste0(run_base_dir,"/", j, "_fish"))
  # source("salmon_IPM.R")
}










# rm(list=ls())
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

