



psi <- array( NA,c(4,4,4) )
for ( season in 1:4 ){
 
  psi[season,,] <- t( pList$psiBeta[season,,] 
                      )
  if (move==F) psi[season,,] <- diag(nRiver)

}
#This is to examine the effect of migration, observed migration is too small for debugging
#psi <- matrix(c(1,0,0,0,.10,.70,.10,.10,.10,.10,.70,.10,.10,.10,.10,.70),4,4)




###### Probability of survival: 
sx<-function(x,y,flow,temp,season,river,params) {
  
  stdX <- (x-params$lengthMean[season,river])/params$lengthSd[season,river] 
  
  u<-invlogit( mean(params$phiBeta[1,season,river,1:nIter]) +
               mean(params$phiBeta[2,season,river,1:nIter])*stdX +
               mean(params$phiBeta[3,season,river,1:nIter])*flow +
               mean(params$phiBeta[4,season,river,1:nIter])*temp +
               mean(params$phiBeta[5,season,river,1:nIter])*flow*temp +
               mean(params$phiBeta[6,season,river,1:nIter])*flow*stdX +
               mean(params$phiBeta[7,season,river,1:nIter])*temp*stdX +
               mean(params$phiBeta[8,season,river,1:nIter])*flow*temp*stdX
              )
  return(u);
}




#Survival from egg to recruit
sjx<-function(x,y,params) {
  
  u <- params$earlySurv+0*x
  return(u);
}

#Growth probabilities
gxy<-function(x,y,flow,temp,season,river,params) {  
  stdX <- (x-params$lengthMean[season,river])/params$lengthSd[season,river]  
  g  <- ( mean(params$grBeta[1,season,river,1:nIter]) +
          mean(params$grBeta[2,season,river,1:nIter])*stdX +
          mean(params$grBeta[3,season,river,1:nIter])*flow +
          mean(params$grBeta[4,season,river,1:nIter])*temp +
          mean(params$grBeta[5,season,river,1:nIter])*flow*temp +
          mean(params$grBeta[6,season,river,1:nIter])*flow*stdX +
          mean(params$grBeta[7,season,river,1:nIter])*temp*stdX +
          mean(params$grBeta[8,season,river,1:nIter])*flow*temp*stdX 
          )
  var <- mean((params$grSigma[season,river,1:nIter])^2)
  sd <- var^(1/2)
  
  return(dnorm(y,g+x,sd))
}

#Number of eggs
f1x <- function(x,y,flow,temp,season,river,params) {
  
  if(season == 2){  
    #expected number of seedlings after establishment
    nKids<-(params$nKidsSizeInt * x ^ params$NKidsSizeSlope)
  }
  else{
    nKids <- x*0
  }
  return(nKids*0.5)   #The *0.5 is to account for the sex ratio. This can be included as another demographic rate if we want later
}


#Size of recruits
f2xy <- function(x,y,season,river,params) {
  if(season == 2){
    kidsize.mean <- params$kidSizeMean[river]
    kidsize.sd <- params$kidSizeSd[river]
    #probability of producing a seedling of size y
    tmp<-dnorm(y,kidsize.mean,kidsize.sd)/(1-pnorm(0,kidsize.mean,kidsize.sd))
    return(tmp)
  }
  else{
    tmp <- x*0
  }
  return(tmp)
}


###### Probability of reproducing
f3x<-function(x,y,season,river,params) {
  if(season == 2){
    #r = ifelse( x < params$rCutoff,0,1 )
    # may want differnet params for obear nad the rest
    r = plogis(x, params$rInt, params$rSlope)
  }
  else{
    r <- x*0
  }
  return(r)
}

