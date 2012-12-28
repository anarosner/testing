    
library(plyr)
library(rjags)
library(ggplot2)
library(abind)
#rjags::load.module("dic")


dMData$length[dMData$tagNumberCH=='1BF1FF6207' & dMData$season == 3 & dMData$year == 2005] <- NA
dMData$length[dMData$tagNumberCH=='1BF1FF6521' & dMData$season == 3 & dMData$year == 2005] <- NA
dMData$length[dMData$tagNumberCH=='1BF18CE7ED' & dMData$season == 2 & dMData$year == 2006] <- NA
dMData$length[dMData$tagNumberCH=='1BF20FF1B9' & dMData$season == 3 & dMData$year == 2005] <- NA
dMData$length[dMData$tagNumberCH=='257C67CA48' ] <- NA
dMData$length[dMData$tagNumberCH=='1BF20EB7A4' & dMData$season == 4 & dMData$year == 2008] <- NA



dMData$riverOrdered <- factor(dMData$river,levels=c('WEST BROOK','WB JIMMY','WB MITCHELL','WB OBEAR'), ordered=T)


dPre <- dMData[ dMData$enc == 1 &
                dMData$season == 3 &
                dMData$age == 0,
                c('tagNumberCH','year','riverOrdered','length')]

# ggplot(dPre, aes(length))+
#   geom_freqpoly(aes(colour=factor(year)), binwidth=5)+
#   facet_grid(~river)





#####
# fdDATA is flood and drought frequencies and durations
fdDATA$year <- as.numeric( fdDATA$year )
fdDATA$year2 <- fdDATA$year
fdDATA$year <- fdDATA$year-min(fdDATA$year) + 1

floodDur <-   array(0,c(4,max(fdDATA$year),max(fdDATA$river)))
droughtDur <- array(0,c(4,max(fdDATA$year),max(fdDATA$river)))
floodFreq <-  array(0,c(4,max(fdDATA$year),max(fdDATA$river)))
discharge <- array(0,c(4,max(fdDATA$year),max(fdDATA$river)))
temperature <- array(0,c(4,max(fdDATA$year),max(fdDATA$river)))

for ( i in 1:nrow(fdDATA) ){
  
    floodDur[fdDATA$season[i],fdDATA$year[i],fdDATA$river[i]] <-    fdDATA$floodDur[i]
  droughtDur[fdDATA$season[i],fdDATA$year[i],fdDATA$river[i]] <-  fdDATA$droughtDur[i]
   floodFreq[fdDATA$season[i],fdDATA$year[i],fdDATA$river[i]] <-   fdDATA$floodFreq[i]
   discharge[fdDATA$season[i],fdDATA$year[i],fdDATA$river[i]] <-   fdDATA$discharge[i]  
 temperature[fdDATA$season[i],fdDATA$year[i],fdDATA$river[i]] <- fdDATA$temperature[i]
}


#####


############ Predictors that are in a matrix have season in rows and river in columns
dPreTag <- within(
       data = list(),
       expr = {
    
  river = as.numeric(dPre$riverOrdered) #-3
  nRivers = length(unique(dPre$river)) #may need to add one for unobs
  lengthDATA = dPre$length
  nRows = nrow(dPre)
#  ind = as.numeric(dMData$tagNumberCH)

  year = dPre$year-min(dPre$year) + 1
  nYears = max(dPre$year)-min(dPre$year)+1


 # temp = temperature


  # fill in 0's and NaN's with across year means ## don't really need this now with scale standardizing below
for ( s in 1:4 ){
  for ( r in 1:nRivers ){
    for ( y in 1:nYears ){

      not0NotNA <- which( temperature[s,,r] != 0 & !is.na(temperature[s,,r]) )
      if ( is.na( temperature[s,y,r] ) | temperature[s,y,r] == 0  ) temperature[s,y,r] <- mean(temperature[s,not0NotNA ,r],na.rm=T) #

      not0NotNA <- which( discharge[s,,r] != 0 & !is.na(discharge[s,,r]) )
      if ( is.na( discharge[s,y,r] ) | discharge[s,y,r] == 0  ) discharge[s,y,r] <- mean( discharge[s,not0NotNA ,r],na.rm=T) #

    }
  }
}

#########
# scale temperature and dsicharge values arcoss years
d2 <- discharge ; t2 <- temperature
d2[d2 == 0] <- NA ; t2[t2 == 0] <- NA

# get z-scores across years. this orders the result as [s,r,y]
d3 <- aaply(d2, c(1,3), scale) ; t3 <- aaply(t2, c(1,3), scale)
# reorder array to [s,y,r]
d3 <- aaply(d3,c(1,3,2),identity) ; t3 <- aaply(t3,c(1,3,2),identity)
d3[is.na(d3)] <- 0 ; t3[is.na(t3)] <- 0
#########

#  floodDur = floodDur
#  droughtDur = droughtDur
#  floodFreq = floodFreq
  discharge = d3 
  temperature = t3

  tenZeros = rep(0,6)
  tenIdentity = diag(6)
 }
)


inits<- function(){
  list(
       )      
  }

   
# MCMC settings
nadapt <- 25000
nburn <- 10000
niter <- 100000
nthin <- 25
nchains <- 1

(beforeAdapt <- Sys.time())
print( beforeAdapt )

preGrowth<- jags.model(
     file = bugsName,  
     data = dPreTag,
 #    inits = inits,
     n.chains = nchains,
     n.adapt = nadapt,             
)

(afterAdapt <- Sys.time())
afterAdapt - beforeAdapt


varsToMonitor<-c(
  
    'meanInt'
  , 'sigmaInt'

  , 'meanBeta'
  , 'sigmaBeta'    
    
#  , 'meanBetaMean'
#  , 'meanBetaSigma'
    
#  , 'sigmaBetaMean'
#  , 'sigmaBetaSigma'

) 

# out1=out  ## for running a second set of iters

( beforeJags <- Sys.time() )
print( beforeJags )

outPre <- jags.samples(
    model = preGrowth,
    variable.names = varsToMonitor,
    n.iter = niter,
    thin = nthin,
    progress.bar = 'text'
  ) 

( done <- Sys.time() )

print(afterAdapt - beforeAdapt) 
print(done - beforeJags)
