#!/usr/bin/env Rscript

library(sp)
library(rgdal)
library(rgeos)
library(maptools)     


# Load NHDplus Data

##
##
#####
####
###
##
#


# Base directory (change depending on file structure wherever this is being run)
basedir<-"C:/ALR/SpatialData"
# basedir<-"/home/ana/testing"
# basedir<-"C:/Documents/__USGS"

##### Define projections
# Projection used by NHD and NHDplus
proj4.NHD<-"+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"
#Projection used by NHDplus for gridded data
# proj4.NHDplusgrid
# Projection used by MassGIS
# proj4.MAstateplane<-"+proj=lcc +lat_1=41.71666666666667 +lat_2=42.68333333333333 +lat_0=41 +lon_0=-71.5 +x_0=200000 +y_0=750000 +ellps=GRS80 +units=m +no_defs"

setwd("C:/ALR/GeneratedSpatialData/FilesByHUC10_3")
system.time(HUC_8<-readShapePoly("HUC_8",proj4string=CRS(proj4.NHD)))



















### load NHDPlus shapefiles and attribute dbf files
#Flowlines
setwd(paste0(basedir,"/NHDPlus/NHDPlusV21_NE_01_NHDSnapshot_01/NHDPlusNE/NHDPlus01/NHDSnapshot/Hydrography"))
system.time(NEFlow.line<-readShapeLines("NHDFlowline",proj4string=CRS(proj4.NHD)))

#Catchments
setwd(paste0(basedir,"/NHDplus/NHDPlusV21_NE_01_NHDPlusCatchment_01/NHDPlusNE/NHDPlus01/NHDPlusCatchment"))
system.time(NECatch.shape<-readShapePoly("Catchment",proj4string=CRS(proj4.NHD)))


# setwd("C:/ALR/GeneratedSpatialData/FilesByHUC10")
# system.time(catchmentwHUC<-readShapePoly("catchmentwHUC",proj4string=CRS(proj4.NHD)))
# huc10<-unique(as.character(catchmentwHUC$HUC_10))
# huc10


#Attributes tables
setwd(paste0(basedir,"/NHDplus/NHDPlusV21_NE_01_NHDPlusAttributes_01/NHDPlusNE/NHDPlus01/NHDPlusAttributes"))
system.time(plusflow<-read.dbf("PlusFlow.dbf"))
system.time(plusflowlineVAA<-read.dbf("PlusFlowlineVAA.dbf"))

names(plusflowlineVAA)
order<-plusflowlineVAA[,c(1,4)]
large<-order[order$StreamOrde>=6 & !is.na(order$StreamOrde),]
# large<-large[!is.na(large$StreamOrde),]

large[1:22,]
dim(large)
large[280:310,]
is.na(large[280:310,])

setwd("C:/ALR/GitHub/testing/spatial_data")
write.csv(large$ComID, file="large_features.csv", row.names=FALSE)



system.time(plusflow<-read.csv("PlusFlow.csv"))
system.time(plusflow<-read.csv("LargeFeatures.csv"))



### Merge w/ VAA table to get attributes
plusdata<-plusflowlineVAA[,c(1,4,1)]
bkup<-NEFlow.line
NEFlow.line@data[c(1:nrow(NEFlow.line@data)),ncol(NEFlow.line)+1]<-c(1:nrow(NEFlow.line@data)) #add column to sort by later
colnames(NEFlow.line@data)[15]<-"sortby"
NEFlow.line@data<-merge(NEFlow.line, plusdata, by.x="COMID", by.y="ComID", all.x=TRUE, sort=FALSE)
NEFlow.line@data<-NEFlow.line@data[order(NEFlow.line$sortby),] 
rm(list=c("plusdata"))

#add centroid
NECatch.shape$lat<-0
NECatch.shape$long<-0
Centroids<-gCentroid(NECatch.shape, byid=TRUE, id = "FEATUREID")

# NHDFlow.line<-NEFlow.line
# NHDCatch.shape<-NECatch.shape

#HUC 8 outlines
#by first 8 digits of reachcode in Flowlines
huc8<-unique(substr(NEFlow.line$REACHCODE,1,8))
unique(substr(NEFlow.line$REACHCODE,1,4))
huc8[34:35]

setwd("C:/ALR/GeneratedSpatialData/FilesByHUC10_3")

# for (i in 34:35) {
for (i in 1:length(huc8)) {
   print(paste("iteration: ",i,"huc:",huc8[i]))
   huc8.line<-NEFlow.line[substr(NEFlow.line$REACHCODE,1,8)==huc8[i],] #HUC8
   huc8.shape<-NECatch.shape[NECatch.shape$FEATUREID %in% huc8.line$COMID,]
   print(paste("number of catchments:",length(huc8.shape)))
   writeOGR(huc8.shape,  ".", layer=paste0(huc8[i],"Catchments"), driver="ESRI Shapefile")
   writeOGR(huc8.line,  ".", layer=paste0(huc8[i],"Flowlines"), driver="ESRI Shapefile")
   writeOGR(huc8.shape,  paste0(huc8[i],"Catchments.kml"), layer="NHDCatchments", driver="KML",dataset_options=c("NameField=FEATUREID"))
   writeOGR(huc8.line,  paste0(huc8[i],"Flowlines.kml"), layer="NHDplusFlowlines", driver="KML",dataset_options=c("NameField=COMID","DescriptionField=GNIS_NAME"))       
}





#set up new blank spatialpolygonsdataframe w/ structure of NECatch.shape
oldcatch<-NECatch.shape
newcatch<-NHDCatch.shape[1,]
newcatch@data
# names(newcatch@data)
newcatch@data[,"HUC_10"]<-1
newcatch<-newcatch[-1,]
newcatch@data
# names(newcatch@data)

featureids<-oldcatch$FEATUREID

setwd("C:/ALR/GeneratedSpatialData/dissolved_hucs")
huc10.shape<-readShapePoly("huc10",proj4string=CRS(proj4.NHD))

length(huc10.shape)
for (i in 1:2) {
    i<-345
i<-10
    
    catch_index<-gCovers(huc10.shape[i,],NECatch.shape)
   #catch_index<-over(huc10.shape[i,],NECatch.shape)

   huc_index<-over(NECatch.shape[catch_index,],huc10.shape)
#     huc10.shape@polygons[[i]]@ID
   plot(huc10.shape[huc_index,])
   plot(NECatch.shape[catch_index,],add=T)
   currentcatch<-newcatch
   currentcatch_index<-length(currentcatch)+1
   newcatch[newcatch_index,]<-oldcatch[catch_index,]
   newcatch[newcatch_index,"HUC_10"]
#    oldcatch<-oldcatch[-oldcatch$FEATUREID==current,]
#    print(paste("old",length(oldcatch),"new",length(newcatch)))
   
}


