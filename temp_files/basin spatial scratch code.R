
basedir<-"C:/ALR/GitHub/testing"
# lat<-42.414712
# long<- -72.629807

lat<-42.369248
long<- -72.417847
toJSON(list(lat=42.540939,long=-72.609808))
testingargs<-list(lat=42.540939,long=-72.609808)
toJSON(list(lat=42.566515,long=-72.630494))
toJSON(list(lat=42.55896,long=-72.622769))
toJSON(list(lat=42.572014,long=-72.630837))
toJSON(list(lat=42.624865,long=-72.90915))

library(rjson)
#number met points w/in basin
#0
toJSON(list(lat=42.45008322,long=-72.38135737))
#1
toJSON(list(lat=42.60978763,long=-72.32909048))
#2
toJSON(list(lat=42.53622681,long=-72.69786247))

#westbrook
toJSON(list(lat=42.414776,long=-72.628398))
testingargs<-list(lat=42.414776,long=-72.628398)



getBasinfromLatLong(42.414712,-72.629807,basedir,output_type="both")
getBasinfromLatLong(42.359146,-72.503593,basedir,output_type="shapefile")
getBasinfromLatLong(42.328569,-72.582471,basedir,output_type="shapefile")


getBasinfromLatLong(42.369248,-72.417847,basedir,output_type="both")
system.time(obj<-getBasinfromLatLong(42.469329, -72.640127,basedir,output_type="both"))




















basedir<-"C:/ALR/GitHub/testing/basins"

#!/usr/bin/env Rscript

# basedir<-"/home/ana/testing" 
#source("setup.R")
NHDCatch.shape<-lowCTCatch.shape
NHDFlow.line<-lowCTFlow.line

temp<-6780817
setwd(paste0("C:/ALR/GitHub/testing/basins/",temp))

basedir<-"C:/ALR/GitHub/testing"
setwd(paste0(basedir,"/rscripts"))
source("spatial_functions.R")


getBasinfromLatLong(42.414712,-72.629807,basedir,output_type="both")
getBasinfromLatLong(42.359146,-72.503593,basedir,output_type="shapefile")
getBasinfromLatLong(42.328569,-72.582471,basedir,output_type="shapefile")
getBasinfromLatLong(42.335707,-72.573502,basedir,output_type="both")

#jscript passes r lat, long
#r finds featureid
#r checks if dir for featureid
#  for now, assumes dir has all needed
#  eventually, check for param.json, COMID.txt, and whichever spatial files specified?
#if dir exists, r returns basinid (featureid)
#if dir does not exist, 
#  r delineates basins, writes needed files, and returns featureid
#if segment too small or too large, returns null













catchments<-NHDCatch.shape[NHDCatch.shape$FEATUREID %in% seg,]
basin<-gUnaryUnion(catchments)
plot(catchments)
plot(basin)

setwd("C:/ALR/SpatialData/NHDplus/NHDPlusV21_NE_01_NHDPlusAttributes_01/NHDPlusNE/NHDPlus01/NHDPlusAttributes")
cumarea<-read.dbf("CumulativeArea.dbf")
names(cumarea)
cumarea[cumarea$ComID==6780817,]
coord<-gCentroid(basin.shape)
coord
spframe<-SpatialPointsDataFrame(coord,data=c(FeatureID=featureID))
SpatialPointsDataFrame(coords, data, coords.nrs = numeric(0),
                       proj4string = CRS(as.character(NA)), match.ID = TRUE, bbox = NULL)
gCentroid(spgeom, byid=FALSE, id = NULL)
basin.shape<-test
row.names(basin.shape)<-"rows"
row.names(basin.shape)<-NULL

catchments<-NHDCatch.shape[NHDCatch.shape$FEATUREID %in% seg,]
basin.shape<-gUnaryUnion(catchments)
centroid<-gCentroid(basin.shape, byid=FALSE, id = NULL)
lat<-attr(centroid,"coords")[,"y"]
lat
long<-attr(centroid,"coords")[,"x"]
long
paste0(lat,", ",long)


attributes(centroid)
attributes(basin.shape)
class(catchments)
class(basin.shape)
class(centroid)

basin.shape<-SpatialPolygonsDataFrame(basin.shape,data=data.frame(StartFID=featureID,row.names=NULL),match.ID=FALSE)
class(basin.shape)
basin.shape@data

centroid<-SpatialPointsDataFrame(centroid,data=data.frame(FeatureID=featureID,row.names=NULL),match.ID=FALSE)
class(centroid)
centroid@data

SpatialPointsDataFrame(coords, data, coords.nrs = numeric(0),
                       proj4string = CRS(as.character(NA)), match.ID = TRUE, bbox = NULL)