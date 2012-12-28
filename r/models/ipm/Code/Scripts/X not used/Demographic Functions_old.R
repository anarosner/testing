

###### Probability of survival: 
sx<-function(x,y,flow,temp,season,river,params) {

  type <- metaRiverA[river,2]
  
  stdX <- (x-params$lengthMean[season,type])/params$lengthSd[season,type]
  u<-invlogit( mean(params$phiBeta[1,season,type,]) +
    mean(params$phiBeta[2,season,type,])*stdX +
    mean(params$phiBeta[3,season,type,])* flow +
    mean(params$phiBeta[4,season,type,])*  temp+
    mean(params$phiBeta[5,season,type,])* flow*temp+
    mean(params$phiBeta[6,season,type,])*  flow*stdX+
    mean(params$phiBeta[7,season,type,])* temp*stdX+
    mean(params$phiBeta[8,season,type,])*   flow*temp*stdX 
               )
  return(u);
}


#Survival from egg to recruit
sjx<-function(x,y,river,flowS1,flowS2,flowS3,flowS4,tempS1,tempS2,tempS3,tempS4,params) {
  
  type <- metaRiverA[river,2]
      
#       u <- invlogit(     (params$earlySurvBeta[type]) + (params$earlySurvBeta[4+yr-2])+x*0)
 
    u<-invlogit( (params$earlySurvBeta[type,1]) +
      (params$earlySurvBeta[type,2])* flowS1 +
      (params$earlySurvBeta[type,3])* tempS1 +
      (params$earlySurvBeta[type,4])* flowS2+
      (params$earlySurvBeta[type,5])* tempS2+
      (params$earlySurvBeta[type,6])* flowS1* tempS1+
      (params$earlySurvBeta[type,7])* flowS2* tempS2+

      x*0
    )
#   u<-invlogit( (params$earlySurvBeta[type]) +
#     (params$earlySurvBeta[5])* flowS1 +
#     (params$earlySurvBeta[6])* tempS1 +
# #     (params$earlySurvBeta[7])* flowS2+
# #     (params$earlySurvBeta[8])* tempS2+
# #     (params$earlySurvBeta[9])* flowS3+
# #     (params$earlySurvBeta[10])* tempS3+
# #     (params$earlySurvBeta[11])* flowS4+
# #     (params$earlySurvBeta[12])* tempS4+
#     (params$earlySurvBeta[7])* flowS1 * tempS1+
# #                (params$earlySurvBeta[14])*  flowS2 * tempS2+
# #                (params$earlySurvBeta[15])* flowS3 * tempS3+
# #                (params$earlySurvBeta[16])* flowS4 * tempS4+
#                x*0
#  )
#   
#     u<-invlogit( 
#       (params$earlySurvBeta[type]) +
#       (params$earlySurvBeta[5])* flowS1 +
#       (params$earlySurvBeta[6])* tempS1 +
# #        (params$earlySurvBeta[7])* flowS2+
# #        (params$earlySurvBeta[8])* tempS2+
# #        (params$earlySurvBeta[9])* flowS3+
# #        (params$earlySurvBeta[10])* tempS3+
# #        (params$earlySurvBeta[11])* flowS4+
# #        (params$earlySurvBeta[12])* tempS4+
#        (params$earlySurvBeta[7])* flowS1^2 +
# #       (params$earlySurvBeta[14])* tempS1^2 +
# #        (params$earlySurvBeta[15])* flowS2^2+
# #        (params$earlySurvBeta[16])* tempS2^2+
# #        (params$earlySurvBeta[17])* flowS3^2+
# #        (params$earlySurvBeta[18])* tempS3^2+
# #        (params$earlySurvBeta[19])* flowS4^2+
# #        (params$earlySurvBeta[20])* tempS4^2+
#                  x*0
#     )
  

 
  return(u)

}
  
#   type <- metaRiverA[river,2]
#   if (type == 4){
#     u <- params$earlySurvObear+0*x}
#   else {
#     u <- params$earlySurv+0*x}
#   
#   return(u);


#Growth probabilities
gxy<-function(x,y,flow,temp,season,river,params) { 
  type <- metaRiverA[river,2]
 
 stdX <- (x-params$lengthMean[season,type])/params$lengthSd[season,type]  
  g  <- ( mean(params$grBeta[1,season,type,]) +
    mean(params$grBeta[2,season,type,])*stdX +
    mean(params$grBeta[3,season,type,])* flow +
    mean(params$grBeta[4,season,type,])*  temp+
     mean(params$grBeta[5,season,type,])* flow*temp#+
#     mean(params$grBeta[6,season,type,])*  flow*stdX+
#     mean(params$grBeta[7,season,type,])* temp*stdX+
#     mean(params$grBeta[8,season,type,])*   flow*temp*stdX
          )
  
#  sd <-  mean(params$grSigmaBeta[season,type,])
  sd <- exp(( mean(params$grSigmaBeta[1,season,type,]) +
    mean(params$grSigmaBeta[2,season,type,])*stdX +
    mean(params$grSigmaBeta[3,season,type,])* flow +
    mean(params$grSigmaBeta[4,season,type,])*  temp #+
#     mean(params$grSigmaBeta[5,season,type,])* flow*temp+
#     mean(params$grSigmaBeta[6,season,type,])*  flow*stdX+
#     mean(params$grSigmaBeta[7,season,type,])* temp*stdX+
#     mean(params$grSigmaBeta[8,season,type,])*   flow*temp*stdX
              ))
   var <- sd^2
  
  return(dnorm(y,g+x,sd))
}

#Number of eggs
f1x <- function(x,y,flow,temp,season,river,params) {
  
  type <- metaRiverA[river,2]
  if(season == 3){  
    
    nKids<-(params$nKidsSizeInt * x ^ params$NKidsSizeSlope)
  }
  else{
    nKids <- x*0
  }
  return(nKids*0.5)   #The *0.5 is to account for the sex ratio. This can be included as another demographic rate if we want later
}


#Size of recruits
f2xy<-function(x,y,flowS1,flowS2,flowS3,flowS4,tempS1,tempS2,tempS3,tempS4,river,params) { 
  type <- metaRiverA[river,2]
  
#   
#   r  <- ( mean(params$kidSizeMeanInt[type,]) +
#     mean(params$kidSizeMeanBeta[1,type,]) * tempS1+
#     mean(params$kidSizeMeanBeta[2,type,]) * tempS2+
#     mean(params$kidSizeMeanBeta[3,type,]) * tempS3+
#     mean(params$kidSizeMeanBeta[4,type,]) * tempS4+
#     mean(params$kidSizeMeanBeta[5,type,]) * flowS1+
#     mean(params$kidSizeMeanBeta[6,type,]) * flowS2+
#     mean(params$kidSizeMeanBeta[7,type,]) * flowS3+
#     mean(params$kidSizeMeanBeta[8,type,]) * flowS4+  
#     
#     mean(params$kidSizeMeanBeta[9,type,]) * tempS1^2+
#     mean(params$kidSizeMeanBeta[10,type,]) * tempS2^2+
#     mean(params$kidSizeMeanBeta[11,type,]) * tempS3^2+
#     mean(params$kidSizeMeanBeta[12,type,]) * tempS4^2+
#     mean(params$kidSizeMeanBeta[13,type,]) * flowS1^2+
#     mean(params$kidSizeMeanBeta[14,type,]) * flowS2^2+
#     mean(params$kidSizeMeanBeta[15,type,]) * flowS3^2+
#     mean(params$kidSizeMeanBeta[16,type,]) * flowS4^2
#  
#   )+x*0
#   var <- exp( mean(params$kidSizeSdInt[type,]) +
#     mean(params$kidSizeSdBeta[1,type,]) * tempS1+
#     mean(params$kidSizeSdBeta[2,type,]) * tempS2+
#     mean(params$kidSizeSdBeta[3,type,]) * tempS3+
#     mean(params$kidSizeSdBeta[4,type,]) * tempS4+
#     mean(params$kidSizeSdBeta[5,type,]) * flowS1+
#     mean(params$kidSizeSdBeta[6,type,]) * flowS2+
#     mean(params$kidSizeSdBeta[7,type,]) * flowS3+
#     mean(params$kidSizeSdBeta[8,type,]) * flowS4+  
#     
#     mean(params$kidSizeSdBeta[9,type,]) * tempS1^2+
#     mean(params$kidSizeSdBeta[10,type,]) * tempS2^2+
#     mean(params$kidSizeSdBeta[11,type,]) * tempS3^2+
#     mean(params$kidSizeSdBeta[12,type,]) * tempS4^2+
#     mean(params$kidSizeSdBeta[13,type,]) * flowS1^2+
#     mean(params$kidSizeSdBeta[14,type,]) * flowS2^2+
#     mean(params$kidSizeSdBeta[15,type,]) * flowS3^2+
#     mean(params$kidSizeSdBeta[16,type,]) * flowS4^2
#   )
#   sd <- var^(1/2)
  
  
  r  <- ( mean(params$kidSizeMeanBeta[type]) +
    mean(params$kidSizeMeanBeta[1+4]) * tempS1 +
    mean(params$kidSizeMeanBeta[2+4]) * flowS1 +
    mean(params$kidSizeMeanBeta[3+4]) * tempS2 +
    mean(params$kidSizeMeanBeta[4+4]) * flowS2 +
    mean(params$kidSizeMeanBeta[5+4]) * tempS3 +
    mean(params$kidSizeMeanBeta[6+4]) * flowS3 +
    mean(params$kidSizeMeanBeta[7+4]) * tempS4 +
    mean(params$kidSizeMeanBeta[8+4]) * flowS4 +
          
    mean(params$kidSizeMeanBeta[9+4]) * tempS1^2 +
    mean(params$kidSizeMeanBeta[10+4]) * flowS1^2 +
    mean(params$kidSizeMeanBeta[11+4]) * tempS2^2 +
    mean(params$kidSizeMeanBeta[12+4]) * flowS2^2 +
    mean(params$kidSizeMeanBeta[13+4]) * tempS3^2 +
    mean(params$kidSizeMeanBeta[14+4]) * flowS3^2 +
    mean(params$kidSizeMeanBeta[15+4]) * tempS4^2 +
    mean(params$kidSizeMeanBeta[16+4]) * flowS4^2
          
  )+x*0
  var <- exp( params$kidSizeSdInt )
  sd <- var^(1/2)
  
  
  
  return(dnorm(y,r,sd))
}



# f2xy <- function(x,y,season,river,params) {
#   type <- metaRiverA[river,2]
#   if(season == 2){
#     kidsize.mean <- params$kidSizeMean[type]
#     kidsize.sd <- params$kidSizeSd[type]
#     
#     tmp<-dnorm(y,kidsize.mean,kidsize.sd)
#     return(tmp)
#   }
#   else{
#     tmp <- x*0
#   }
#   return(tmp)
# }


###### Probability of reproducing
f3x<-function(x,y,flowS1,flowS2,flowS3,flowS4,tempS1,tempS2,tempS3,tempS4,river,season,params) {
  type <- metaRiverA[river,2]
  if(season == 3){
    r <- ( mean(params$kidSizeMeanBeta[type]) +
      mean(params$kidSizeMeanBeta[1+4]) * tempS1 +
      mean(params$kidSizeMeanBeta[2+4]) * flowS1 +
      mean(params$kidSizeMeanBeta[3+4]) * tempS2 +
      mean(params$kidSizeMeanBeta[4+4]) * flowS2 +
      mean(params$kidSizeMeanBeta[5+4]) * tempS3 +
      mean(params$kidSizeMeanBeta[6+4]) * flowS3 +
      mean(params$kidSizeMeanBeta[7+4]) * tempS4 +
      mean(params$kidSizeMeanBeta[8+4]) * flowS4 +
      
      mean(params$kidSizeMeanBeta[9+4]) * tempS1^2 +
      mean(params$kidSizeMeanBeta[10+4]) * flowS1^2 +
      mean(params$kidSizeMeanBeta[11+4]) * tempS2^2 +
      mean(params$kidSizeMeanBeta[12+4]) * flowS2^2 +
      mean(params$kidSizeMeanBeta[13+4]) * tempS3^2 +
      mean(params$kidSizeMeanBeta[14+4]) * flowS3^2 +
      mean(params$kidSizeMeanBeta[15+4]) * tempS4^2 +
      mean(params$kidSizeMeanBeta[16+4]) * flowS4^2
    )
    var <- exp( params$kidSizeSdInt )
    sd <- var^(1/2)
    
    rInt <- r+2*sd
    r = plogis(x, rInt, params$rSlope)
  }
  else{
    r <- x*0
  }
  return(r)
}

