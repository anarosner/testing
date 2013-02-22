library(sp)
library(rgdal)
library(rgeos)
library(maptools)     

#load huc 8 outlines
proj4.NHDplus<-"+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"

#"USA_Contiguous_Albers_Equal_Area_Conic_USGS_version"
proj4.LCAD<-"+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23.0 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs "
#"USA_Contiguous_Albers_Equal_Area_Conic"
proj4.otherAEA<-"+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs "

setwd("C:/Documents/__USGS/SpatialData/NHDPlus/NHDPlusV21_NE_01_NHDPlusCatchment_01/NHDPlusNE/NHDPlus01/NHDPlusCatchment")
catchments<-readShapePoly("Catchment",proj4string=CRS(proj4.NHDplus))

