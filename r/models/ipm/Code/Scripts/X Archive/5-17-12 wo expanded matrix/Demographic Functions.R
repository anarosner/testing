
#Movement for Fish
psiA <- array( NA,c(4,4,4) )
for ( season in 1:4 ){
  
  psiA[season,,] <- t( pList$psiBeta[season,,] 
                      )
  if (move==F) psi[season,,] <- diag(nRiver)
  
}

#Movement for Eggs. Sounds stupid, but needs to be there.
psiE <- diag(1,4,4)





##This is to eliminate the probability of fish moving into Olber
if (OlberOff==T){
for (season in 1:4){
  psiA[season,4,1:3] <- 0
  psiA[season,,] <- (psiA[season,,]+t(((1-colSums(psiA[season,,]))/3) * array(1,c(4,4))))
  psiA[season,4,1:3] <- 0
 }
}




###### Probability of survival: 
sx<-function(x,y,flow,temp,season,river,params) {
  if (firstSeason == 0){
    fl  <- 0
    te <- 0
  }
  
  else {
  if (river==pertriver){  
    
  if (season == firstSeason){
    fl  <- flow
    te <- temp
  }
  else {
    fl <- 0
    te <- 0
    }
  }
  else {
    fl <- 0
    te <- 0
  }
  }
  
  stdX <- (x-params$lengthMean[season,river])/params$lengthSd[season,river]
  u<-invlogit( mean(params$phiBeta[1,season,river,1:nIter]) +
    mean(params$phiBeta[2,season,river,1:nIter])*stdX +
    mean(params$phiBeta[3,season,river,1:nIter])*fl +
    mean(params$phiBeta[4,season,river,1:nIter])*te +
    mean(params$phiBeta[5,season,river,1:nIter])*fl*te +
    mean(params$phiBeta[6,season,river,1:nIter])*fl*stdX +
    mean(params$phiBeta[7,season,river,1:nIter])*te*stdX +
    mean(params$phiBeta[8,season,river,1:nIter])*fl*te*stdX
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
     if (firstSeason == 0){
      fl  <- 0
      te <- 0
    }
    
    else {
      if (river==pertriver){  
        
        if (season == firstSeason){
          fl  <- flow
          te <- temp
        }
        else {
          fl <- 0
          te <- 0
        }
      }
      else {
        fl <- 0
        te <- 0
      }
    }
 stdX <- (x-params$lengthMean[season,river])/params$lengthSd[season,river]  
  g  <- ( mean(params$grBeta[1,season,river,1:nIter]) +
          mean(params$grBeta[2,season,river,1:nIter])*stdX +
          mean(params$grBeta[3,season,river,1:nIter])*fl +
          mean(params$grBeta[4,season,river,1:nIter])*te +
          mean(params$grBeta[5,season,river,1:nIter])*fl*te +
          mean(params$grBeta[6,season,river,1:nIter])*fl*stdX +
          mean(params$grBeta[7,season,river,1:nIter])*te*stdX +
          mean(params$grBeta[8,season,river,1:nIter])*fl*te*stdX 
          )
  var <- mean((params$grSigma[season,river,1:nIter])^2)
  sd <- var^(1/2)
  
  return(dnorm(y,g+x,sd))
}

#Number of eggs
f1x <- function(x,y,flow,temp,season,river,params) {
  
  if(season == 2){  
    
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
    
    tmp<-dnorm(y,kidsize.mean,kidsize.sd)
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
    r = plogis(x, params$rInt, params$rSlope)
  }
  else{
    r <- x*0
  }
  return(r)
}

