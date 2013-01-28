setwd("C:/ALR/GitHub/testing/temp_files/from Ron")
wb<-read.csv("Westbrook Lambdas - normalized.csv")
wb[1:10,]
spring<-wb[wb$Season=="Spring",c(3:5)]
summer<-wb[wb$Season=="Summer",c(3:5)]
fall<-wb[wb$Season=="Fall",c(3:5)]
winter<-wb[wb$Season=="Winter",c(3:5)]

spring[1:10,]

get.spring<-function(x,y) {
   spring[spring$StandardizedFlow==x & spring$StandardizedTemperature==y,"Lambda"]
}
get.summer<-function(x,y) {
   summer[summer$StandardizedFlow==x & summer$StandardizedTemperature==y,"Lambda"]
}get.fall<-function(x,y) {
   fall[fall$StandardizedFlow==x & fall$StandardizedTemperature==y,"Lambda"]
}get.winter<-function(x,y) {
   winter[winter$StandardizedFlow==x & winter$StandardizedTemperature==y,"Lambda"]
}

get.spring(-1.5,-1.5)
get.spring(0.75,-1.5)
