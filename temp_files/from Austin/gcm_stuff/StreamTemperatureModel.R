Stream.Temperature <- function(coef_str,met.st) {

coefs <- coef_str

## Use model to predict stream temperature values for 2000-2080
futureStrTemp <- as.data.frame(matrix(NA, nrow=length(met.st[,1]), ncol=4))
names(futureStrTemp) <- c("WB","Jimmy","Mitchell","Obear")

for (i in 1:ncol(futureStrTemp)){
  futureStrTemp[,i] <- coefs[4,i]+(coefs[1,i]-coefs[4,i])/(1+exp((4*tan(coefs[3,i])/(coefs[1,i]-coefs[4,i]))*(coefs[2,i]-met.st[,3])))
}

futureStrTempFinal <- cbind(met.st[,1], met.st[,2], futureStrTemp)
names(futureStrTempFinal) <- c("YEAR", "MONTH", "WB","Jimmy","Mitchell","Obear")

########################################################################
## Calculating seasonal mean stream temperatures for population model ##
########################################################################
futureStrTempFinal$Season[futureStrTempFinal$MONTH == "1"] <- "4"  # winter
futureStrTempFinal$Season[futureStrTempFinal$MONTH == "2"] <- "4"
futureStrTempFinal$Season[futureStrTempFinal$MONTH == "3"] <- "4"
futureStrTempFinal$Season[futureStrTempFinal$MONTH == "4"] <- "1"  # spring
futureStrTempFinal$Season[futureStrTempFinal$MONTH == "5"] <- "1"
futureStrTempFinal$Season[futureStrTempFinal$MONTH == "6"] <- "2"  # summer
futureStrTempFinal$Season[futureStrTempFinal$MONTH == "7"] <- "2"
futureStrTempFinal$Season[futureStrTempFinal$MONTH == "8"] <- "2"
futureStrTempFinal$Season[futureStrTempFinal$MONTH == "9"] <- "3"  # fall
futureStrTempFinal$Season[futureStrTempFinal$MONTH == "10"] <- "3"
futureStrTempFinal$Season[futureStrTempFinal$MONTH == "11"] <- "3"
futureStrTempFinal$Season[futureStrTempFinal$MONTH == "12"] <- "4"

agg <- list(futureStrTempFinal[,1],futureStrTempFinal[,7])
mon.final <- array(NA,c(600,4))

for (i in 1:4) {
tmp <- aggregate(futureStrTempFinal[,i+2],agg,mean)
mon.final[,i] <- tmp[order(tmp$Group.1,tmp$Group.2),3]
}
return(cbind(as.numeric(tmp[order(tmp$Group.1,tmp$Group.2),1]),as.numeric(tmp[order(tmp$Group.1,tmp$Group.2),2]),as.numeric(round(mon.final[,1],2)),as.numeric(round(mon.final[,2],2)),as.numeric(round(mon.final[,3],2)),as.numeric(round(mon.final[,4],2))))

}