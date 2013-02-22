#!/usr/bin/env Rscript
print("starting simulate drought script")

this_dir<-getwd()
source("./supporting_scripts/param_startup.R")
source("./supporting_scripts/plot_functions.R")
source("./supporting_scripts/Hargreaves.R") #This generates ET values and may get moved to the met section

GCMDays <- read.table(paste0(args$data_dir,"/flow_data/Days_For_ABCDE_GCM.txt"),header=TRUE)


#parse user input params
change_precip_mean <- parse.param("precip_mean_y1")/100+1
change_temp_mean <- parse.param("temp_mean_y1")
drought_annual_inches <- parse.param("drought_annual_inches")
# percentile<-parse.param("percentile")/100

#load data
basin_id <- parse.param("basin_id")
basin_param <- fromJSON(file=paste0(basin_dir,"/",basin_id,"/param.json"))


h.precipannual<-read.csv(paste0(basin_dir,"/",basin_id,"/precipannualh.csv"),header=T)
s.precipannual<-read.csv(paste0(basin_dir,"/",basin_id,"/precipannual",change_precip_mean,".csv"),header=T)
s.precip<-read.csv(paste0(basin_dir,"/",basin_id,"/precip",change_precip_mean,".csv"),header=T)
# tempannual<-read.csv(paste0(basin_dir,"/",basin_id,"/tempannualh.csv"),header=T)

sort1<-h.precipannual[order(h.precipannual$precip.in),]
sort2<-s.precipannual[order(s.precipannual$precip.in),]
hist(sort2[,"precip.in"],breaks=seq(from=floor(min(sort2$precip.in)/10)*10, to=ceiling(max(sort2$precip.in)/10)*10, by=2),freq=F,border="red",add=T)
hist(sort1[-1,"precip.in"],breaks=seq(from=floor(min(sort1$precip.in[-1])/10)*10, to=ceiling(max(sort1$precip.in)/10)*10, by=2),freq=F,add=F,border="blue")
quantile(h.precipannual$precip.in,c(.01,.1,.25,.5,.75,.9,.99))
quantile(s.precipannual$precip.in,c(.01,.1,.25,.5,.75,.9,.99))


plot(s.precipannual$water.year,s.precipannual$precip.in,xlim=c(min(h.precipannual$water.year),max(s.precipannual$water.year)))
abline(h=median(s.precipannual$precip.in))
points(h.precipannual$water.year,h.precipannual$precip.in,col="red")
abline(h=median(h.precipannual$precip.in),col="red")



# value<-sort[round(nrow(sort)*percentile)-1,"precip.in"]
# value

i<-ceiling(runif(1,min=0,max=nrow(precipannual)))
flag<-0
while (flag==0) {
#    print(i)
#    print(precipannual$precip.in[i])
   {
   if ( precipannual$precip.in>=floor(value-1) & precipannual$precip.in<=ceiling(value+1) )
      flag<-1
   else { 
      if (i+1>nrow(precipannual) )
         i<-1
      else
         i<-i+1
      }
   }
}
print(i)
print(precipannual$precip.in[i])


precip<-read.csv(paste0(basin_dir,"/",basin_id,"/precipannual.csv"),header=T)

