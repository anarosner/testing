#!/usr/bin/env Rscript
print("starting dummy land use script")

this_dir<-getwd()
source("./supporting_scripts/param_startup.R")
# source("./supporting_scripts/plot_functions.R")

#parse user input params
# scenario <- parse.param("scenario")

setwd(run_dir)
svg(filename="thumbnail.svg",width=2.5,height=2.5)
plot(1,1)
dev.off()

# #load data
# basin_id <- parse.param("basin_id")
# basin_param <- fromJSON(file=paste0(basin_dir,"/",basin_id,"/param.json"))
