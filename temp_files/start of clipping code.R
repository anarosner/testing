library(sp)
library(rgdal)
library(rgeos)
library(maptools)     

#load huc 8 outlines
proj4.NHDplus<-"+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"

proj4.LCAD<-"+proj=aea +x_0=0 +y_0=0 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +lat_0=23.0 +units=m +datum=NAD83 +ellps=GRS80  +no_defs "
# Projection: Albers
# False_Easting: 0.000000
# False_Northing: 0.000000
# Central_Meridian: -96.000000
# Standard_Parallel_1: 29.500000
# Standard_Parallel_2: 45.500000
# Latitude_Of_Origin: 23.000000
# Linear Unit: Meter
# Datum: D_North_American_1983
# #"USA_Contiguous_Albers_Equal_Area_Conic_USGS_version"
# proj4.LCAD<-"+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23.0 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs "
# #"USA_Contiguous_Albers_Equal_Area_Conic"
# proj4.otherAEA<-"+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs "

setwd("C:/ALR/StreamData/NHDplus/NHDPlusV21_NE_01_NHDPlusCatchment_01/NHDPlusNE/NHDPlus01/NHDPlusCatchment")
# setwd("C:/Documents/__USGS/SpatialData/NHDPlus/NHDPlusV21_NE_01_NHDPlusCatchment_01/NHDPlusNE/NHDPlus01/NHDPlusCatchment")
catchments<-readShapePoly("Catchment",proj4string=CRS(proj4.NHDplus))

setwd("C:/ALR/LandscapeData/postland_poly_projected")
postland<-readShapePoly("postland2010",proj4string=CRS(proj4.NHDplus))
# postland2<-postland