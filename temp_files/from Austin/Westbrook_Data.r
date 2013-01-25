setwd("/home/polebitski/R_Files/Westbrook")
daterange <- read.table("date_range.txt",header=FALSE)
source("StreamTemperatureModel.R")
source("StreamFlowModel.R")

setwd("/home2/VIC_MET/CCRUNS")

coef_str <- matrix(c(17.9591113,18.8820565,19.0037266,18.9207433,
 8.5984435, 9.9418541, 8.9901715, 9.7464999, 
 0.7068154,  0.6790824,  0.7429935,  0.6796490,
 -0.1901492, -0.5812414, -0.4318731, -0.5826941),ncol=4,byrow=TRUE)

parameters <- c(0.934, 350.2514, 0.0000014, 0.62498, 0.4623, -0.7021, 9.9952) #SCE
namo <- "data_42.4375_-72.6875"

data.length <- 54787*4

inc1 <- seq(from=1,to=data.length,by=4)
inc2 <- seq(from=2,to=data.length,by=4)
inc3 <- seq(from=3,to=data.length,by=4)
inc4 <- seq(from=4,to=data.length,by=4)


#600 season periods by 112 scenarios by 4 seasons by 2 variables
final_flow <- array(NA,c(600,112))
final_temp <- array(NA,c(600,112,4))
final_met <- array(NA,c(1800,112,3))

for(i in 1:75) {
	setwd("/home2/VIC_MET/CCRUNS/A2")
	files <- list.files()
	dirnames <- paste("/home2/VIC_MET/CCRUNS/A2/",files[i],sep="")
	setwd(dirnames)
	gcmdata <- readBin(namo, integer(), n = data.length, size = 2, endian = "little")
	max.temp <- gcmdata[inc2]/100
	min.temp <- gcmdata[inc3]/100
	prcp <- gcmdata[inc1]/40
	max.temp.month <- aggregate(max.temp,FUN=mean,by=list(daterange[,2],daterange[,1]))
	min.temp.month <- aggregate(min.temp,FUN=mean,by=list(daterange[,2],daterange[,1]))
	prcp.month <- aggregate(prcp,FUN=sum,by=list(daterange[,2],daterange[,1]))
	met <- cbind(max.temp.month[,3]-min.temp.month[,3],prcp.month[,3],(max.temp.month[,3]+min.temp.month[,3])/2)
	met.st <- cbind(daterange[,1],daterange[,2],(max.temp+min.temp)/2)
	final_met[,i+37,1] <- max.temp.month[,3] 
	final_met[,i+37,2] <- min.temp.month[,3]
	final_met[,i+37,3] <- prcp.month[,3]
	final_temp[,i+37,] <- Stream.Temperature(coef_str,met.st)[,3:6]
	final_flow[,i+37] <- Stream.Flow(parameters,met)[,3]
	}

setwd("/home/polebitski/R_Files/Westbrook")	
save(final_flow,file="flow.Rdata")
save(final_met,file="met.Rdata")
save(final_temp,file="temp.Rdata")