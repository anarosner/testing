#!/usr/bin/env Rscript

# basedir<-"C:/ALR/GitHub/testing"
basedir<-"/home/ana/testing"
setwd(paste0(basedir,"/rscripts"))
source("spatial_functions.R")


#args consists of single json object which contains lat and long coordinates, and an optional basin nickname
args<-fromJSON( commandArgs(trailingOnly = TRUE) )

if (!is.null(args$nickname))
   getBasinfromLatLong(args$lat,args$long,basedir,nickname=args$nickname,output_type="both")
else
   getBasinfromLatLong(args$lat,args$long,basedir,output_type="both")


