####Goes through a list of basins (GAGES II) and aggregate met data
         # library(sp)
         # library(rgdal)
         # library(maptools)     # loads sp library too


         # #Read in file to use for Hargreaves ET computation
         # setwd("")  #Set this to the DATA directory...
         # VIC.shape<-readShapePoints("VIC_Grid")
         
         # setwd("")  #Set this to the JSON directory...
         # Catch.shape<-readShapePoly("UserDefinedBasin")

###  Ana's changes
# save basin shape as Catch.shape, name used in Austin & Kyle's script
Catch.shape<-basin.shape
# load shapefile of points, in metdata directory
setwd(paste0(args$datadir,"/metdata"))
VIC.shape<-readShapePoints("VIC_Grid")
#temporarily, set to metdata directory in Austin's directory,
#until it is moved to the correct place in node.js
met.dir<-paste0(args$datadir,"/metdata/east")
this.basin.dir<-paste0(args$basindir,"/",featureID)


##Get polygons and aggregate data
temp.points = overlay(VIC.shape,Catch.shape)
	
	#Begin if statement to check if points are in basin
	#If basin has grid points then do aggreagating routine
if(length(which(temp.points==1)) > 0) {
	met.list <- VIC.shape[which(temp.points==1),]	
	if(length(met.list) == 1) {
		temp.name <- paste("data_",met.list$lat,"_",met.list$long,sep="")
		setwd(met.dir) #Set this to the Daily Met Data File
		temp.met <- read.table(temp.name,header=FALSE)
		setwd(this.basin.dir) #Set this to the directory where data needs to get written for climate data
		write.table(temp.met,"met_data.txt",row.names=FALSE,col.names=FALSE,sep=" ")

	}else{
		temp.name <- paste("data_",met.list$lat[1],"_",met.list$long[1],sep="")
		setwd(met.dir) #ana added this.  we need to specify the met directory here, right?
		temp.data <- read.table(temp.name,header=FALSE)
		num_site <- length(met.list)
		temp.met <- array(NA,c(dim(temp.data)[1],7))
		SITE_PRCP <- array(NA,c(dim(temp.data)[1],num_site))
		SITE_TMAX <- array(NA,c(dim(temp.data)[1],num_site))
		SITE_TMIN <- array(NA,c(dim(temp.data)[1],num_site))
		SITE_WIND <- array(NA,c(dim(temp.data)[1],num_site))
		
		for (i in 1:num_site) {	
			temp.name <- paste("data_",met.list$lat[i],"_",met.list$long[i],sep="")
			CUR_SITE_DATA <- read.table(temp.name,header=FALSE)
			SITE_PRCP[,i] <- CUR_SITE_DATA[,4]
			SITE_TMAX[,i] <- CUR_SITE_DATA[,5]
			SITE_TMIN[,i] <- CUR_SITE_DATA[,6]
			SITE_WIND[,i] <- CUR_SITE_DATA[,7]	
		}

		temp.met[,4] <- apply(SITE_PRCP,1,median)
		temp.met[,5] <- apply(SITE_TMAX,1,mean)
		temp.met[,6] <- apply(SITE_TMIN,1,mean)
		temp.met[,7] <- apply(SITE_WIND,1,mean)

		temp.met[,1] <- temp.data[,1]
		temp.met[,2] <- temp.data[,2]
		temp.met[,3] <- temp.data[,3]
		
		setwd(this.basin.dir) #Set this to the directory where data needs to get written for climate data
		write.table(temp.met,"met_data.txt",row.names=FALSE,col.names=FALSE,sep=" ")
	}
#Find nearest grid point to basin centroid b/c no points in basin
}else{
	a1 <- abs(VIC.shape$lat)/57.29577951	#put everything in radians I think...
	b1 <- abs(VIC.shape$long)/57.29577951
	   #change to use centroid points calculated in delineate basin script
      		# 	a2<- abs(Catch.shape$LAT_CENT)/57.29577951
      		# 	b2<- abs(Catch.shape$LONG_CENT)/57.29577951
	a2<- abs(lat)/57.29577951
	b2<- abs(long)/57.29577951

	h <- 6371*acos(round(sin(a1)*sin(a2)+cos(a1)*cos(a2)*cos(b1-b2),10)) #use law of cosines to compute distance 6371 km is radius of earth I think...
	temp.grid <- which(h == min(h))
	temp.name <- paste("data_",VIC.shape$lat[temp.grid],"_",VIC.shape$long[temp.grid],sep="")
	setwd(met.dir) #Set this to the Daily Met Data File
	temp.met <- read.table(temp.name,header=FALSE)
	setwd(this.basin.dir) #Set this to the directory where data needs to get written for climate data
	write.table(temp.met,"met_data.txt",row.names=FALSE,col.names=FALSE,sep=" ")
} 
       