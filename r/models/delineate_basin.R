#!/usr/bin/env Rscript
print("starting basin delineation script")
savedwd<-getwd()
setwd("../..")
basedir<-getwd()



library(sp)
library(rgdal)
library(rgeos)
library(maptools)     
library(rjson)

args<-list(lat=42.491991,long=-72.639097)
# args<-fromJSON( commandArgs(trailingOnly = TRUE) )

lat<-args$lat
long<-args$long



output_type<-"both"
nickname<-""



#load huc 8 outlines
proj4.NHD<-"+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs"
setwd(paste0(basedir,"/spatial_data"))
HUC_8<-readShapePoly("HUC_8",proj4string=CRS(proj4.NHD))
print("read hucs")

#create spatialpoint object from coordinates (coordinates are listed in the order long, lat)
point<-SpatialPoints(matrix(data=c(long,lat),ncol=2,byrow=T), proj4string=CRS(proj4.NHD))

#get huc that contains the point
selected.huc<-over(point,HUC_8)$HUC_8
#    print(selected.huc)

#load catchments for that huc
catchments<-readShapePoly(paste0(selected.huc,"Catchments"),proj4string=CRS(proj4.NHD))
print("read catchments")
featureID<-over(point,catchments)$FEATUREID
   
   ### defines basin from user-chosen
   ### NHDplus stream feature or catchment 
   ### (stream feature's ComID == catchment's FeatureID)


#if basin has already been delimited above that feature, return the ID and do not recreate file
setwd(paste0(basedir,"/basins/"))
{
if (file.exists(as.character(featureID))) {
   #       setwd(savedwd)
#    print("repeat")
   message(toJSON(list(featureID=featureID)))
   print(toJSON(list(featureID=featureID)))
}

else {
   
   setwd(paste0(basedir,"/spatial_data/"))
   
   #checks to see if selected main river, upstream of which would be a basin too big to work with
   largefeatures<-read.csv("LargeFeatures.csv")
   {
   if (featureID %in% largefeatures) {
      message("Please choose a smaller stream or tributary")
   }

   else {
   
      #read in flowlines for the huc this feature is in
      #and the plusflow table, indicating feature connections
      flowlines<-readShapeLines(paste0(selected.huc,"Flowlines"),proj4string=CRS(proj4.NHD))   
      plusflow<-read.csv("PlusFlow.csv")
      print("read flowlines")
      
      #iteratively select all features upstream of user chosen feature
      segments<-c() #list of flowline segments to save
      queue<-c(featureID) #queue of flowline segments that need to be traced upstream                 
      
      while (length(queue)>0) {
         {
         if(queue[1]==0) {
            queue<-queue[-1] #discard 1st element in the queue if it's a zero
            #in the NHDplus tables, features with FROMCOMID==0 have no inflowing tribs
         }
         else {
            segments<-c(segments,queue[1]) #add 1st element to the saved segments
            queue<-c(queue[-1],plusflow[plusflow$TOCOMID==queue[1],]$FROMCOMID) #remove 1st segment from the queue, 
            #add all segments that flow into it to the queue
         }}
      }
      
      #collect all stream segments in upstream network
      stream.line<-flowlines[flowlines$COMID %in% segments,]
      #collect all catchments around upstream network
      catchment.shape<-catchments[catchments$FEATUREID %in% segments,]
      #dissolve border for basin outline
      basin.shape<-gUnaryUnion(catchment.shape)
      #get centroid to write to param file
      centroid<-gCentroid(basin.shape, byid=FALSE, id = NULL)
      #turn outline into spatial dataframe again to enable output as shapefile/kml
      basin.shape<-SpatialPolygonsDataFrame(basin.shape,data=data.frame(StartFID=featureID,row.names=NULL),match.ID=FALSE)

      #create directory for new basin
      setwd(paste0(basedir,"/basins/"))
      dir.create(as.character(featureID))
      setwd(paste0(basedir,"/basins/",featureID))
      
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
      {
      if (nickname=="")
         param<-toJSON(list(basinid=featureID,area=area,lat=lat,long=long))
      else
         param<-toJSON(list(basinid=featureID,area=area,lat=lat,long=long,nickname=nickname))
      }
      write(param, file = "param.json", ncolumns=1,sep="")
      
      print(toJSON(list(featureID=featureID)))
      message(toJSON(list(featureID=featureID)))
      
   }}#end else if feature isn't too large
}} #end else if dir doesn't exist    
   
   







