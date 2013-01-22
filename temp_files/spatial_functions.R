#!/usr/bin/env Rscript

library(sp)
library(rgdal)
library(rgeos)
library(maptools)     
library(rjson)


# Load Basin Deliniation Functions

##
##
#####
####
###
##
#

getBasinfromComID<-function(featureID,HUC_8,basedir,output_type="kml",nickname="") {
   ### defines basin from user-chosen
   ### NHDplus stream feature or catchment 
   ### (stream feature's ComID == catchment's FeatureID)
   
#    savedwd<-getwd()

   #if basin has already been delimited above that feature, return the ID and do not recreate file
   setwd(paste0(basedir,"/basins/"))
#    print(paste("directory exists:",file.exists(as.character(featureID))))
   if (file.exists(as.character(featureID))) {
      #       setwd(savedwd)
      print("repeat")
      return(toJSON(list(featureID=featureID)))
   }
   
   #create directory for new basin
   dir.create(as.character(featureID))
   
   setwd(paste0(basedir,"/spatial_data/"))
   
   #checks to see if selected main river, upstream of which would be a basin too big to work with
   largefeatures<-read.csv("LargeFeatures.csv")
#    print(paste("is large feature:",featureID %in% largefeatures))
   if (featureID %in% largefeatures) {
      message("Please choose a smaller stream or tributary")
      return(NULL)
   }
   

   #read in flowlines for the huc this feature is in
   #and the plusflow table, indicating feature connections
   proj4.NHD<-"+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"
   flowlines<-readShapeLines(paste0(HUC_8,"Flowlines"),proj4string=CRS(proj4.NHD))   
   catchments<-readShapePoly(paste0(HUC_8,"Catchments"),proj4string=CRS(proj4.NHD))
   plusflow<-read.csv("PlusFlow.csv")
#    print("read in flowlines")
   
   setwd(paste0(basedir,"/basins/",featureID))
   
   #iteratively select all features upstream of user chosen feature
   segments<-c() #list of flowline segments to save
   queue<-c(featureID) #queue of flowline segments that need to be traced upstream                 
   
   while (length(queue)>0) {
      if(queue[1]==0) {
         queue<-queue[-1] #discard 1st element in the queue if it's a zero
         #in the NHDplus tables, features with FROMCOMID==0 have no inflowing tribs
      }
      else {
         segments<-c(segments,queue[1]) #add 1st element to the saved segments
         queue<-c(queue[-1],plusflow[plusflow$TOCOMID==queue[1],]$FROMCOMID) #remove 1st segment from the queue, 
         #add all segments that flow into it to the queue
      }
   }
#    print(paste("segments",as.character(segments)))
   
   #collect all stream segments in upstream network
   stream.line<-flowlines[flowlines$COMID %in% segments,]
#    print("collect flowlines")
   #collect all catchments around upstream network
   catchment.shape<-catchments[catchments$FEATUREID %in% segments,]
#    print("collect catchments")
   #dissolve border for basin outline
   basin.shape<-gUnaryUnion(catchment.shape)
#    print("union")
   #get centroid to write to param file
   centroid<-gCentroid(basin.shape, byid=FALSE, id = NULL)
#    print("centroid")
   #turn outline into spatial dataframe again to enable output as shapefile/kml
   basin.shape<-SpatialPolygonsDataFrame(basin.shape,data=data.frame(StartFID=featureID,row.names=NULL),match.ID=FALSE)
   
   
   #export spatial files of basin outline and flowlines
   if (output_type=="kml" | output_type=="both") {         
      writeOGR(basin.shape,  "BasinOutline.kml", layer="BasinOutline", driver="KML",dataset_options=c("NameField=FEATUREID"))
      writeOGR(stream.line,  "NHDplusFlowlines.kml", layer="NHDplusFlowlines", driver="KML",dataset_options=c("NameField=COMID","DescriptionField=GNIS_NAME"))    
   }
   if (output_type=="shapefile" | output_type=="both") {               
      writeOGR(basin.shape,  ".", layer="BasinOutline", driver="ESRI Shapefile")
      writeOGR(stream.line,  ".", layer="NHDplusFlowlines", driver="ESRI Shapefile")
   }
   
   #export text file of comIDs, and param.json file with basin attributes
   write.csv(segments, file="featureID.csv", row.names=FALSE)
   area<-area<-sum(catchment.shape$AreaSqKM)
   lat<-attr(centroid,"coords")[,"y"]
   long<-attr(centroid,"coords")[,"x"]
   if (nickname=="")
      param<-toJSON(list(basinid=featureID,area=area,lat=lat,long=long))
   else
      param<-toJSON(list(basinid=featureID,area=area,lat=lat,long=long,nickname=nickname))
   write(param, file = "param.json", ncolumns=1,sep="")
   
#    setwd(savedwd)
   return(toJSON(list(featureID=featureID)))
   
   
} #end getBasinfromComID


getBasinfromLatLong<-function(lat,long,basedir,output_type="kml",nickname="") {
   savedwd<-getwd()

   #load huc 8 outlines
   proj4.NHD<-"+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"
   setwd(paste0(basedir,"/spatial_data"))
   HUC_8<-readShapePoly("HUC_8",proj4string=CRS(proj4.NHD))
   
   #create spatialpoint object from coordinates (coordinates are listed in the order long, lat)
   point<-SpatialPoints(matrix(data=c(long,lat),ncol=2,byrow=T), proj4string=CRS(proj4.NHD))
   
   #get huc that contains the point
   selected.huc<-over(point,HUC_8)$HUC_8
#    print(selected.huc)
   
   #load catchments for that huc
   catchments<-readShapePoly(paste0(selected.huc,"Catchments"),proj4string=CRS(proj4.NHD))
#    system.time(flowlines<-readShapeLines(paste0(selected.huc,"Flowlines"),proj4string=CRS(proj4.NHD)))
   featureID<-over(point,catchments)$FEATUREID
#    print(featureID)
   
   #call previous function to find basin from the NHDplus flowline or catchment feature

   toreturn<-getBasinfromComID(featureID,HUC_8=as.character(selected.huc),basedir=basedir,output_type=output_type,nickname=nickname)
   setwd(savedwd)
   return(toreturn)
   
}




basedir<-"C:/ALR/GitHub/testing"
# lat<-42.414712
# long<- -72.629807

lat<-42.369248
long<- -72.417847

getBasinfromLatLong(42.414712,-72.629807,basedir,output_type="both")
getBasinfromLatLong(42.359146,-72.503593,basedir,output_type="shapefile")
getBasinfromLatLong(42.328569,-72.582471,basedir,output_type="shapefile")


getBasinfromLatLong(42.369248,-72.417847,basedir,output_type="both")
system.time(obj<-getBasinfromLatLong(42.469329, -72.640127,basedir,output_type="both"))


