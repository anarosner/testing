s.clim<-s.clim[s.clim$YEAR>2010,]

h.clim$season<-0
h.clim[h.clim$MONTH %in% c(3,4,5),"season"]<-1
h.clim[h.clim$MONTH %in% c(6,7,8),"season"]<-2
h.clim[h.clim$MONTH %in% c(9,10,11),"season"]<-3
h.clim[h.clim$MONTH %in% c(12,1,2),"season"]<-4
h.clim[h.clim$MONTH==12,"YEAR"]<-h.clim[h.clim$MONTH==12,"YEAR"]+1

s.clim$season<-0
s.clim[s.clim$MONTH %in% c(3,4,5),"season"]<-1
s.clim[s.clim$MONTH %in% c(6,7,8),"season"]<-2
s.clim[s.clim$MONTH %in% c(9,10,11),"season"]<-3
s.clim[s.clim$MONTH %in% c(12,1,2),"season"]<-4
s.clim[s.clim$MONTH==12,"YEAR"]<-s.clim[s.clim$MONTH==12,"YEAR"]+1

h.seasonal<-aggregate(h.clim,by=list(h.clim$YEAR,h.clim$season),FUN=mean)
s.seasonal<-aggregate(s.clim,by=list(s.clim$YEAR,s.clim$season),FUN=mean)

h.spring<-h.seasonal[h.seasonal$season==1,c('YEAR',"PRCP")]
s.spring<-s.seasonal[s.seasonal$season==1,c('YEAR',"PRCP")]

h.fall<-h.seasonal[h.seasonal$season==3,c('YEAR',"PRCP")]
s.fall<-s.seasonal[s.seasonal$season==3,c('YEAR',"PRCP")]


a<-""
for ( i in 1:nrow(h.spring) ) {
  a<-paste0(a,"[ ",h.spring[i,"YEAR"],",",h.spring[i,"PRCP"]," ] , ")
}
a

b<-""
for ( i in 1:nrow(s.spring) ) {
  b<-paste0(b,"[ ",s.spring[i,"YEAR"],",",s.spring[i,"PRCP"]," ] , ")
}
b


c<-""
for ( i in 1:nrow(h.fall) ) {
  c<-paste0(c,"[ ",h.fall[i,"YEAR"],",",h.fall[i,"PRCP"]," ] , ")
}
c

d<-""
for ( i in 1:nrow(s.fall) ) {
  d<-paste0(d,"[ ",s.fall[i,"YEAR"],",",s.fall[i,"PRCP"]," ] , ")
}
d


# h.spring<-h.clim[,c(2,1,3)]
# s.spring<-s.clim[,c(2,1,3)]

formatC(h.clim[1,"MONTH"],width=2,flag="0")

%m %Y

a<-""
for ( i in 1:nrow(h.clim) ) {
  a<-paste0(a,"[ ",formatC(h.clim[i,"MONTH"],width=2,flag="0"),"-01-",h.clim[i,"YEAR"],",",h.clim[i,"PRCP"]," ] , ")
}
a

b<-""
for ( i in 1:nrow(s.clim) ) {
 b<-paste0(b,"[ ",formatC(s.clim[i,"MONTH"],width=2,flag="0"),"-01-",s.clim[i,"YEAR"],",",s.clim[i,"PRCP"]," ] , ")
}
b
