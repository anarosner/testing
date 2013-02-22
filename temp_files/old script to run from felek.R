testingargs<-list(basin_dir="/home/ana/testing/somepath/basins",
                  run_dir="/home/ana/testing/somepath2/runs/no_change_flow",
                  data_dir="/home/ana/testing/somepath/data")
setwd("/home/ana/testing/r")
source("StreamFlowModel.R")

testingargs<-list(basin_dir="/home/ana/testing/somepath/basins",
                  run_dir="/home/ana/testing/somepath2/runs/no_change_streamtemp",
                  data_dir="/home/ana/testing/somepath/data")
setwd("/home/ana/testing/r")
source("StreamTemperatureModel.R")

testingargs<-list(basin_dir="/home/ana/testing/somepath/basins",
                  run_dir="/home/ana/testing/somepath2/runs/no_change_fish",
                  data_dir="/home/ana/testing/somepath/data")
setwd("/home/ana/testing/r")
source("salmon_IPM.R")

