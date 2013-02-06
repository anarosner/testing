#!/usr/bin/env Rscript
print("starting salmon IPM script")

this_dir<-getwd()
source("./supporting_scripts/param_startup.R")
# source("./supporting_scripts/plot_functions.R")

#parse user input params
stocking_n <- parse.param("stocking_n")
stocking_stage <- parse.param("stocking_stage") 
#directories w/ preceding run outputs
flow_dir <- preceding$flow_dir
streamtemp_dir <- preceding$streamtemp_dir


source(paste0(this_dir,"./supporting_scripts/salmon_IPM/ATS.R"))
source(paste0(this_dir,"./supporting_scripts/salmon_IPM/graphA.R"))
