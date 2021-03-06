
setwd("C:/Users/Scott/Documents/WEATHER_GENERATOR_FOR_ONLINE_TOOL")
mu_prcp <- 1.5
var_prcp <- 1
mu_temp <- 5
directory <- "C:/Users/Scott/Documents/WEATHER_GENERATOR_FOR_ONLINE_TOOL"
Stoc.Weather(mu_prcp,var_prcp,mu_temp,directory)


Stoc.Weather <- function(mu_prcp,var_prcp,mu_temp,directory) {

library(MASS)

#mean_prcp_change_percent <- 1
#var_prcp_change_percent <- 1
#mean_temp_change_celsius <- 0

num_year_sim <- 81
thresh <- 0    #0 mm is threshold for a day to be considered as rain

mean_prcp_change_percent <- mu_prcp
var_prcp_change_percent <- var_prcp
mean_temp_change_celsius <- mu_temp

##########################################################################################################################################################################################
#setwd("/whatever/")
filenames <- "data_42.4375_-72.6875"
num_site <- 1

DATA <- read.table(filenames,header=FALSE)
SITE_PRCP <- array(NA,c(dim(DATA)[1],num_site))
SITE_TMAX <- array(NA,c(dim(DATA)[1],num_site))
SITE_TMIN <- array(NA,c(dim(DATA)[1],num_site))
SITE_TEMP <- array(NA,c(dim(DATA)[1],num_site))
SITE_WIND <- array(NA,c(dim(DATA)[1],num_site))

for (i in 1:num_site) {	
	CUR_SITE_DATA <- read.table(filenames[i],header=FALSE)
	SITE_PRCP[,i] <- CUR_SITE_DATA[,4]
	SITE_TMAX[,i] <- CUR_SITE_DATA[,5]
	SITE_TMIN[,i] <- CUR_SITE_DATA[,6]
	SITE_TEMP[,i] <- (CUR_SITE_DATA[,5] +  CUR_SITE_DATA[,6])/2
	SITE_WIND[,i] <- CUR_SITE_DATA[,7]	
}

DATA[,4] <- apply(SITE_PRCP,1,median)
DATA[,5] <- apply(SITE_TMAX,1,mean)
DATA[,6] <- apply(SITE_TMIN,1,mean)
DATA[,7] <- apply(SITE_WIND,1,mean)

colnames(DATA) <- c("YEAR","MONTH","DAY","PRCP","TMAX","TMIN","WIND")

YEAR <- DATA[,1]
MONTH <- DATA[,2]
DAY <- DATA[,3]
DATE <- as.Date(paste(YEAR,"-",MONTH,"-",DAY,sep=""))
MONTH_DAY <- cbind(MONTH,DAY)
PRCP <- DATA[,4]
TMAX <- DATA[,5]
TMIN <- DATA[,6]
TEMP <- (TMAX + TMIN)/2

##########################################################################################################################################################################################




##########################################################################################################################################################################################
KNN <- function(cur_sim_PRCP,cur_sim_TEMP,PRCP_TODAY,TEMP_TODAY,PRCP_TOMORROW,TEMP_TOMORROW,TMAX_TOMORROW,TMIN_TOMORROW,DATE_TOMORROW,k,mean_monthly_PRCP,mean_monthly_TEMP) {
	w_PRCP <- 4/mean_monthly_PRCP
	w_TEMP <- 0/mean_monthly_TEMP
	var_order <- 1:length(PRCP_TODAY)
	distance <- sqrt(w_PRCP*(cur_sim_PRCP - PRCP_TODAY)^2 + w_TEMP*(cur_sim_TEMP - TEMP_TODAY)^2)
	ordered_distances <- matrix(cbind(var_order,distance)[order(distance),],ncol=2)
	K_Distances <- matrix(ordered_distances[1:k,],ncol=2)
	PROBS <- (1/row(K_Distances)[,1]) / sum((1/row(K_Distances)[,1]))
	selection <- sample(row(K_Distances)[,1],size=1,prob=PROBS,replace=TRUE)
	FINAL_PRCP <- PRCP_TOMORROW[K_Distances[selection,1]]
	FINAL_TEMP <- TEMP_TOMORROW[K_Distances[selection,1]]
	FINAL_TMAX <- TMAX_TOMORROW[K_Distances[selection,1]]
	FINAL_TMIN <- TMIN_TOMORROW[K_Distances[selection,1]]
	FINAL_DATE <- DATE_TOMORROW[K_Distances[selection,1]]
	return(c(FINAL_PRCP,FINAL_TEMP,FINAL_TMAX,FINAL_TMIN,FINAL_DATE))
}
##########################################################################################################################################################################################


###############################################################GET REGIONAL TRANSITION PROBABILITIES############################################################################################
PRCP_LAG0 <- PRCP[2:length(PRCP)]
PRCP_LAG1 <- PRCP[1:(length(PRCP)-1)]
MONTH_LAG0 <- MONTH[2:length(PRCP)]
MONTH_LAG1 <- MONTH[1:(length(PRCP)-1)]
p01 <- array(NA,12)
p11 <- array(NA,12)

for (m in 1:12) {
		x <- which(MONTH_LAG1==m)
		CUR_PRCP0 <- PRCP_LAG0[x]
		CUR_PRCP1 <- PRCP_LAG1[x]
		p01[m] <- length(which(PRCP_LAG1[x]<=thresh & PRCP_LAG0[x]>thresh)) / length(which(PRCP_LAG1[x]<=thresh))
		p11[m] <- length(which(PRCP_LAG1[x]>thresh & PRCP_LAG0[x]>thresh)) / length(which(PRCP_LAG1[x]>thresh))
} 
######################################################################################################################################################################

###################################################################################STOCHASTIC SIMULATION###############################################################
START_YEAR_SIM <- 2000
END_YEAR_SIM <- START_YEAR_SIM + num_year_sim - 1
DATE_SIM <- seq(as.Date(paste(START_YEAR_SIM,"-01-01",sep="")),as.Date(paste(END_YEAR_SIM,"-12-31",sep="")),by="day")
DAY_SIM <- as.numeric(format(DATE_SIM,"%d"))
MONTH_SIM <- as.numeric(format(DATE_SIM,"%m"))
YEAR_SIM <- as.numeric(format(DATE_SIM,"%Y"))
SIM_LENGTH <- length(DATE_SIM)
OCCURENCES <- array(0,c(SIM_LENGTH))
SIM_PRCP <- array(0,c(SIM_LENGTH))
SIM_TEMP <- array(25,c(SIM_LENGTH))     
SIM_TMAX <- array(30,c(SIM_LENGTH))     
SIM_TMIN <- array(20,c(SIM_LENGTH))     
SIM_DATE <- array(as.Date("1980-01-01"),c(SIM_LENGTH))     #we start simulated temperature at mean January temperature

mean_prcp_change_percent2 <- seq(1,mean_prcp_change_percent,length.out=num_year_sim)
var_prcp_change_percent2 <- seq(1,var_prcp_change_percent,length.out=num_year_sim)
mean_temp_change_celsius2 <- seq(0,mean_temp_change_celsius,length.out=SIM_LENGTH)

#MARKOV CHAIN
count <- 1
for (j in 1:SIM_LENGTH) {
	m <- MONTH_SIM[j]
	count <- count + 1
	if (count <=SIM_LENGTH) {
		rn <- runif(1,0,1)
		if (OCCURENCES[(count-1)]==0) {pp <- p01[m]}
		if (OCCURENCES[(count-1)]==1) {pp <- p11[m]}
		if(rn <= pp) {
			OCCURENCES[count] <- 1
		} else {
			OCCURENCES[count] <- 0
		}
	}
}
OCCURENCES[1] <- 0    #ensure first day is marked dry, which will align with first prcp ammount as 0


count <- 1
for (j in 1:SIM_LENGTH) {
	m <- MONTH_SIM[j]
	d <- DAY_SIM[j]
	if (count < SIM_LENGTH) {
		cur_OCCERENCE <- OCCURENCES[count]
		next_OCCURENCE <- OCCURENCES[(count+1)]
	
		cur_day <- which(MONTH_DAY[,1]==m & MONTH_DAY[,2]==d)
		cur_day <- c((cur_day-3),(cur_day-2),(cur_day-1),cur_day,(cur_day+1),(cur_day+2),(cur_day+3))
		cur_day <- subset(cur_day,cur_day > 0)
				
		if (cur_OCCERENCE==0 & next_OCCURENCE==0) {cur_day_cur_state <- which(PRCP[cur_day]<=thresh & PRCP[(cur_day+1)]<=thresh)}
		if (cur_OCCERENCE==1 & next_OCCURENCE==0) {cur_day_cur_state <- which(PRCP[cur_day]>thresh & PRCP[(cur_day+1)]<=thresh)}
		if (cur_OCCERENCE==0 & next_OCCURENCE==1) {cur_day_cur_state <- which(PRCP[cur_day]<=thresh & PRCP[(cur_day+1)]>thresh)}
		if (cur_OCCERENCE==1 & next_OCCURENCE==1) {cur_day_cur_state <- which(PRCP[cur_day]>thresh & PRCP[(cur_day+1)]>thresh)}
			
		possible_days <- cur_day[cur_day_cur_state]
		PRCP_TODAY <- PRCP[possible_days]
		TEMP_TODAY <- TEMP[possible_days]
		PRCP_TOMORROW <- PRCP[possible_days+1]
		TEMP_TOMORROW <- TEMP[possible_days+1]
		TMAX_TOMORROW <- TMAX[possible_days+1]
		TMIN_TOMORROW <- TMIN[possible_days+1]
		DATE_TOMORROW <- DATE[possible_days+1]

		cur_sim_PRCP <- SIM_PRCP[count]
		cur_sim_TEMP <- SIM_TEMP[count]
					
		mm <- which(MONTH==m)
		mean_monthly_TEMP <- mean(TEMP[mm],na.rm=TRUE)
		mean_monthly_PRCP <- mean(PRCP[mm],na.rm=TRUE)
	
		k <- round(sqrt(length(possible_days)))
		RESULT <- KNN(cur_sim_PRCP,cur_sim_TEMP,PRCP_TODAY,TEMP_TODAY,PRCP_TOMORROW,TEMP_TOMORROW,TMAX_TOMORROW,TMIN_TOMORROW,DATE_TOMORROW,k,mean_monthly_PRCP,mean_monthly_TEMP)

		SIM_PRCP[(count+1)] <- RESULT[1]
		SIM_TEMP[(count+1)] <- RESULT[2]
		SIM_TMAX[(count+1)] <- RESULT[3]
		SIM_TMIN[(count+1)] <- RESULT[4]
		SIM_DATE[(count+1)] <- as.Date(RESULT[5],origin="1970-01-01")
					
		count <- count+1
	}
}

###########################################################################################################################################################################


############################################################################TIME SERIES ADJUSTMENTS########################################################################

#temperature adjustment
SIM_TEMP_ADJUSTED <- SIM_TEMP + mean_temp_change_celsius2
SIM_TMIN_ADJUSTED <- SIM_TMIN + mean_temp_change_celsius2
SIM_TMAX_ADJUSTED <- SIM_TMAX + mean_temp_change_celsius2


#precipitation adjustment
probs <- seq(0.001,.999,by=0.001)
parameters <- array(NA,c(12*num_year_sim,2))
ratios <- array(NA,c(length(probs),12*num_year_sim))
prcp_distr <- array(NA,c(length(probs),12*num_year_sim))
prcp_distr2 <- array(NA,c(length(probs),12*num_year_sim))

for (y in 1:num_year_sim) {
	for (m in 1:12) {
		a <- which(MONTH==m & PRCP>thresh)
		dist1 <- fitdistr(PRCP[a],"gamma")
		parameters[(12*(y-1)+m),] <- c(dist1$estimate[[1]],dist1$estimate[[2]])
		prcp_distr[,(12*(y-1)+m)] <- qgamma(probs,shape=dist1$estimate[[1]], rate = dist1$estimate[[2]])
		shp2 <- (mean_prcp_change_percent2[y])^2*dist1$estimate[[1]]/var_prcp_change_percent2[y]
		rate2 <- mean_prcp_change_percent2[y]*dist1$estimate[[2]]/var_prcp_change_percent2[y]
		prcp_distr2[,(12*(y-1)+m)] <- qgamma(probs,shape=shp2, rate = rate2)
		ratios[,(12*(y-1)+m)] <- prcp_distr2[,(12*(y-1)+m)]/prcp_distr[,(12*(y-1)+m)]
	}
}

SIM_PRCP_ADJUSTED <- SIM_PRCP
for (y in 1:num_year_sim) {
	for(m in 1:12) {
		w <- which(SIM_PRCP>thresh & MONTH_SIM==m & YEAR_SIM==(START_YEAR_SIM:END_YEAR_SIM)[y])
		for (j in 1:length(w)) {		
			multiplier <- approx(prcp_distr[,(12*(y-1)+m)],ratios[,(12*(y-1)+m)],SIM_PRCP[w[j]])$y
			if (is.na(multiplier)==TRUE) {multiplier <- 1}
			SIM_PRCP_ADJUSTED[w[j]] <- multiplier*SIM_PRCP[w[j]]
		}
	}
}
###########################################################################################################################################################################


#########################################################################WRITE OUT FILES FOR FURTHER USE####################################################################

FINAL_STOCHASTIC_DAILY <- cbind(YEAR_SIM,MONTH_SIM,DAY_SIM,round(SIM_PRCP_ADJUSTED,2),round(SIM_TMAX_ADJUSTED,2),round(SIM_TMIN_ADJUSTED,2),round(SIM_TEMP_ADJUSTED,2))
FINAL_STOCHASTIC_MONTHLY1 <- aggregate(FINAL_STOCHASTIC_DAILY[,c(1:2,5:7)],FUN=mean,by=list(FINAL_STOCHASTIC_DAILY[,2],FINAL_STOCHASTIC_DAILY[,1]))[,3:7]
FINAL_STOCHASTIC_MONTHLY2 <- aggregate(FINAL_STOCHASTIC_DAILY[,4],FUN=sum,by=list(FINAL_STOCHASTIC_DAILY[,2],FINAL_STOCHASTIC_DAILY[,1]))[,3]

FINAL_STOCHASTIC_MONTH1 <- aggregate(FINAL_STOCHASTIC_DAILY[,c(1:2,5:7)],FUN=mean,by=list(FINAL_STOCHASTIC_DAILY[,2]))[,c(1,4:6)]
FINAL_STOCHASTIC_MONTH2 <- aggregate(FINAL_STOCHASTIC_DAILY[,4],FUN=sum,by=list(FINAL_STOCHASTIC_DAILY[,2]))[,2]/num_year_sim

FINAL_STOCHASTIC_MONTHLY <- cbind(FINAL_STOCHASTIC_MONTHLY1[,1:2],round(FINAL_STOCHASTIC_MONTHLY2,2),round(FINAL_STOCHASTIC_MONTHLY1[,3:5],2))
FINAL_STOCHASTIC_MONTH <- cbind(FINAL_STOCHASTIC_MONTH1[,1],FINAL_STOCHASTIC_MONTH2,FINAL_STOCHASTIC_MONTH1[,2:4])

colnames(FINAL_STOCHASTIC_DAILY) <- c("YEAR","MONTH","DAY","PRCP","TMAX","TMIN","TAVG")
colnames(FINAL_STOCHASTIC_MONTHLY) <- c("YEAR","MONTH","PRCP","TMAX","TMIN","TAVG")
colnames(FINAL_STOCHASTIC_MONTH) <- c("MONTH","PRCP","TMAX","TMIN","TAVG")

##Read in historic data, set directory to unique one and write stuff out
hist <- read.table("month_stochastic_weather_historic.txt",header=FALSE)
# setwd("/home/austin/WebStuff/Images/")
# setwd(directory)

write.table(FINAL_STOCHASTIC_DAILY,"daily_stochastic_weather.txt",row.names=FALSE,quote=FALSE)
write.table(FINAL_STOCHASTIC_MONTHLY,"monthly_stochastic_weather.txt",row.names=FALSE,quote=FALSE)
write.table(FINAL_STOCHASTIC_MONTH,"month_stochastic_weather.txt",row.names=FALSE,quote=FALSE,col.names=FALSE)

max.y.p <- max(hist[,2],FINAL_STOCHASTIC_MONTH$PRCP)+25
min.y.p <- min(hist[,2],FINAL_STOCHASTIC_MONTH$PRCP)-25

max.y.t <- max(hist[,5],FINAL_STOCHASTIC_MONTH$TAVG)+5
min.y.t <- min(hist[,5],FINAL_STOCHASTIC_MONTH$TAVG)-5

##Create a image for website
png(filename="StocWeather.png",width=725, height=575, bg="white")
op <- par(mfrow=c(2,2))
plot(hist[,1],hist[,2],col="black",type="b",pch=1,ylim=c(min.y.p,max.y.p),ylab="Precipitation (mm)",xlab="Month",main="Mean Stochastic Weather - Precipitation")
lines(FINAL_STOCHASTIC_MONTH$MONTH,FINAL_STOCHASTIC_MONTH$PRCP,col="red")
plot(hist[,1],hist[,5],col="black",type="b",pch=1,ylim=c(min.y.t,max.y.t),ylab="Temperature (C)",xlab="Month",main="Mean Stochastic Weather - Temperature")
lines(FINAL_STOCHASTIC_MONTH$MONTH,FINAL_STOCHASTIC_MONTH$TAVG,col="red")
plot(1:length(FINAL_STOCHASTIC_MONTHLY$PRCP),FINAL_STOCHASTIC_MONTHLY$PRCP,type="l")
plot(1:length(FINAL_STOCHASTIC_MONTHLY$PRCP),FINAL_STOCHASTIC_MONTHLY$TAVG,type="l")
par(op)
dev.off()

###########################################################################################################################################################################
}