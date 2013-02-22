#This code runs the Mohseni model and plots the output.
rm(list=ls())

setwd("C:/Users/STUDENT/Documents/Stream Temp/Stream Temp Data/MA_Data")													#>>>> INPUT REQUIRED <<<<
load("Daily_MA_WB_AirTemps.Rdata")																							#>>>> INPUT REQUIRED <<<<
load("Daily_MA_WB_StreamTemps.Rdata")																						#>>>> INPUT REQUIRED <<<<

library(zoo)
library(rjags)

##To use this you need air and stream temperature##########################################################################
##You also need to decide whether to fit two models to account for hysteresis in warming/cooling periods

###Calibrate Using SCE#######################################
###Mohseni Model Formulated for SCE Calibration#################
ST_SCE_CALIBRATE <- function(parameters,AirObs,StreamObs) {
mu <- parameters[1]
alpha <- parameters[2]
theta <- parameters[3]
betas <- parameters[4]

STemp.mod <- mu+(alpha-mu)/(1+exp((4*tan(theta)/(alpha-mu))*(betas-AirObs)))

notna <- which(is.na(StreamObs) == FALSE)				
top1 = sum((StreamObs[notna]-STemp.mod[notna])^2)
bottom1 = sum((StreamObs[notna]-mean(StreamObs[notna]))^2)
nash <- 1-top1/bottom1
RMSE <- sqrt(top1/(length(notna)))
		
return(RMSE)
}

setwd("C:/Users/STUDENT/Dropbox/Stream Temperature/StreamTemp_Model")
source("SCE.R")
##Calib Num is length of run
#set initial parameter values
start_par <- c(7,20,0.5,5)

#set lower and upper bounds on possible parameter values
lowerb <- c(0,5,0.00001,0)   
upperb <- c(15,41,1,30) 
parms <- array(NA,c(length(met.data[1,,1])-1,7))

##Select station to calibrate and run optimizer
##Here is where you give the function Air Temp (Air.T) and Stream Temp (Stream.T)
for(i in 2:length(met.data[1,,1])) {
Air.T <- met.data[,i,1]
Stream.T <- temp.array[,i,1]
out <- SCEoptim(ST_SCE_CALIBRATE,start_par,Air.T,Stream.T,lower=lowerb,upper=upperb)
parms[i-1,1:4] <- out$par	
parms[i-1,5] <- out$value		
}

###Use model to check fit#######################
###Below is the Mohseni model but fed a set of parameters and returns the fit
ST_MAN_CALIBRATE <- function(parameters,AirT) {
mu <- parameters[1]
alpha <- parameters[2]
theta <- parameters[3]
betas <- parameters[4]
AirT <- AirT

STemp.mod <- mu+(alpha-mu)/(1+exp((4*tan(theta)/(alpha-mu))*(betas-AirT)))
return(STemp.mod)
}



##################################################################################################################
##Extract best fit parameters and run Manual Mohseni model to check fit visually. Also returns calibration metrics

setwd("C:/Workspace/Mohseni")
#List of Lat Long coords for the sites:
sites <- read.table("MA_WB_locations.txt", header=TRUE)																		#>>>> INPUT REQUIRED <<<<
num_sites <- length(met.data[1,,1])-1

calibrate <- matrix(NA, nrow=num_sites,ncol=9)

calibrate[1:num_sites,1] <- (sites[1:num_sites,2])
calibrate[1:num_sites,2] <- (sites[1:num_sites,3])
calibrate[1:num_sites,3] <- (sites[1:num_sites,4])
rownames(calibrate) <- (sites[1:num_sites,1])
colnames(calibrate) <- as.character(c("Latitude","Longitude","Days","Nash","RMSE","mu","alpha","theta","beta"))

#Loop through to get all calibration metrics
for (i in 2:length(met.data[1,,1])){
model.parm <- parms[i-1,1:4]
Air.T <- met.data[,i,1]
Stream.T <- temp.array[,i,1]
stream.est <- ST_MAN_CALIBRATE(model.parm,Air.T)

Obs<-temp.array[,i,1]
notna <- which(is.na(Obs) == FALSE)				
top1 = sum((Obs[notna]-stream.est[notna])^2)
bottom1 = sum((Obs[notna]-mean(Obs[notna]))^2)

nash1 <- round((1-top1/bottom1), digits = 3)
RMSE1 <- round((sqrt(top1/(length(notna)))), digits = 3)

calibrate[i-1,4] <- nash1
calibrate[i-1,5] <- RMSE1
}

calibrate[,6:9] <- (parms[,1:4])

nash2 <- sprintf("%.3f", nash1)
RMSE2 <- sprintf("%.3f", RMSE1)

calibrate


#Plotting:

#Where to save the plot:
setwd("C:/Users/STUDENT/Documents/Stream Temp/MODEL_Mohseni et al/MA")														#>>>> INPUT REQUIRED <<<<

x.name <- c("","","","","","","","","","","","","","","","","","","","","Stream Temp (deg C)","Stream Temp (deg C)","Stream Temp (deg C)","Stream Temp (deg C)","Stream Temp (deg C)")
y.name <-c("Air Temp (deg C)","","","","","Air Temp (deg C)","","","","","Air Temp (deg C)","","","","","Air Temp (deg C)","","","","","Air Temp (deg C)","","","","")

#This loop automatically saves the 5x5 plot files. Enter the number of these files you wish to create for j (multiple of 25).
for(j in 1:1){		
png(filename=paste0("MA_WB_(",j,").png"),width=2175, height=1425, bg="white")												#>>>> INPUT REQUIRED <<<<

beg <- (j*25-24)
if((j*25) >= length(calibrate[,1])) fin <- length(calibrate[,1]) else fin <- (j*25)

op<-par(mfrow=c(5,5))#,mar=c(4,4,4,2),oma=c(10,2,2,2))
for (i in beg:fin){	
model.parm <- parms[i,1:4]
Air.T <- met.data[,i+1,1]
Stream.T <- temp.array[,i+1,1]
stream.est <- ST_MAN_CALIBRATE(model.parm,Air.T)

plot(sort(Air.T),sort(stream.est), type="l", main=paste0("Site: ", sites$Site[i]," |  Lat: ",sites$Latitude[i]," | Lon: ",sites$Longitude[i]), xlab=x.name[i-1], ylab=y.name[i-1], xlim=c(-10,30), ylim=c(-15,30),lwd=3)
points(Air.T, Stream.T)
text(5,-10, labels = c(paste0("Nash = ",calibrate[i,4],"       RMSE = ",calibrate[i,5])))
}
dev.off()
}
