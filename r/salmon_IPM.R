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

print("starting ATS.R")
source(file = file.path(this_dir,"supporting_scripts/salmon_IPM/ATS.R"))
print("starting graphA.R")
with(data=list(run_dir=run_dir), expr = source(file = file.path(this_dir,"supporting_scripts/salmon_IPM/graphA.R"), local=TRUE))
print("starting graphB.R")
with(data=list(run_dir=run_dir), expr = source(file = file.path(this_dir,"supporting_scripts/salmon_IPM/graphB.R"), local=TRUE))
print("starting graphC.R")
with(data=list(run_dir=run_dir), expr = source(file = file.path(this_dir,"supporting_scripts/salmon_IPM/graphC.R"), local=TRUE))
print("starting graphD.R")
with(data=list(run_dir=run_dir), expr = source(file = file.path(this_dir,"supporting_scripts/salmon_IPM/graphD.R"), local=TRUE))




