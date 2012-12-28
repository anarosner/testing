
###### Probability of survival: 
sx<-function(x,y,flow,temp,season,river,params) {
  
  stdX <- (x-params$lengthMean[season,river])/params$lengthSd[season,river] 
  
  u<-invlogit(params$phiBeta[1,season,river] +
    params$phiBeta[2,season,river]*stdX +
    params$phiBeta[3,season,river]*flow +
    params$phiBeta[4,season,river]*temp #+
              #       params$phiBeta[5,season,river]*flow*temp +
              #       params$phiBeta[6,season,river]*flow*stdX +
              #       params$phiBeta[7,season,river]*temp*stdX +
              #       params$phiBeta[8,season,river]*flow*temp*stdX
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
  g  <- ( params$grBeta[1,season,river] +
    params$grBeta[2,season,river]*stdX +
    params$grBeta[3,season,river]*flow +
    params$grBeta[4,season,river]*temp #+
          #    params$grBeta[5,season,river]*flow*temp +
          #    params$grBeta[6,season,river]*flow*stdX +
          #    params$grBeta[7,season,river]*temp*stdX +
          #    params$grBeta[8,season,river]*flow*temp*stdX 
          )
  
  return(dnorm(y,g+x,params$grSigma[season,river]))
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


#============================================================================================#
# (II) Construct the component matrices 
#============================================================================================#


# n.big.matrix is the number of mesh points for size, nRiver is the number of rivers 
n.big.matrix = 100; nRiver = pList$nRiver;

##### Put all component matrices into 3-dimensional arrays 
Sx <-   array(NA,c(n.big.matrix,n.big.matrix,4,nRiver))
SJx <-  array(NA,c(n.big.matrix,n.big.matrix,4,nRiver))
Gxy <-  array(NA,c(n.big.matrix,n.big.matrix,4,nRiver))
F1x <-  array(NA,c(n.big.matrix,n.big.matrix,4,nRiver))
F2xy <- array(NA,c(n.big.matrix,n.big.matrix,4,nRiver))
F3x <-  array(NA,c(n.big.matrix,n.big.matrix,4,nRiver))


# minimum and maximum sizes 
minsize= 40; maxsize=300; L= minsize; U= maxsize;
n = n.big.matrix 

# boundary points b and mesh points y
b = L+c(0:n)*(U-L)/n; 
y = 0.5*(b[1:n]+b[2:(n+1)]);

# step size for midpoint rule, see equations 4 and 5
h = y[2]-y[1]

#Create  matrices, looping over age  
for (river in 1:nRiver) {
  for (season in 1:4){
    # Note the use of outer to compute each component matrix without looping.    
    #Note that h is only multiplied by demographic rates that are probability distributions (F2xy and Gxy)
    F1x[,,season,river] <-   t(outer(y,y,f1x,flow=flow,temp=temp,season=season,river=river,params=pList))
    F2xy[,,season,river] <- h*t(outer(y,y,f2xy,season=season,river=river,params=pList))
    F3x[,,season,river] <-   t(outer(y,y,f3x,season=season,river=river,params=pList))
    Sx[,,season,river] <-   t(outer(y,y,sx,flow=flow,temp=temp,season=season,river=river,params=pList))  
    SJx[,,season,river] <-   t(outer(y,y,sjx,params=pList))
    Gxy[,,season,river] <- h*t(outer(y,y,gxy,flow=flow,temp=temp,season=season,river=river,params=pList))
    
  }
}

#============================================================================================#
# (III) Create the function 'getEig', which is used to calculate w and v analytically
#       getEigsens should be used to calculate sensitivities
#============================================================================================#

#This creates the function 'getEig', which is used to calculate w and v analytically. NOte these are rescaled values.
#Should not be used to calculate sensitivities or elasticities
getEig <- function(mat,transpose=F){
  
  vec <- Re(eigen(mat)$vectors[,1])                        #This calculates the right eigenvector 
  if(transpose==T) vec <- Re(eigen(t(mat))$vectors[,1])   #This calculates the left eigenvector 
  
  vec <- vec/(2*range(vec)[2]);                          #This standardizes the values by the largest value
  return(vec)
}

getEigsens <- function(mat,transpose=F){                  #This one can be used to calculate sensitivities and elasticities
  
  vec <- Re(eigen(mat)$vectors[,1])                        #This calculates the right eigenvector 
  if(transpose==T) vec <- Re(eigen(t(mat))$vectors[,1])    #This calculates the left eigenvector 
  return(vec)
}



#============================================================================================#
# (III) Here is the main model for asymptotic values
#============================================================================================#

######Controls for adjusting migration. F turns off migration.
move <- T

psi <- array( NA,c(4,4,4) )
for ( season in 1:4 ){
  psi[season,,] <- t( pList$psiBeta#[season,,] 
                      )
  if (move==F) psi[season,,] <- diag(nRiver)
}
#This is to examine the effect of migration, observed migration is too small for debugging
#psi <- matrix(c(1,0,0,0,.10,.70,.10,.10,.10,.10,.70,.10,.10,.10,.10,.70),4,4)

######## Create Kernels for each stream and season. Indexed by season and river#####################
K <- array(NA,c(length(y),length(y),4,nRiver))
#Construct the kernel 
for (river in 1:nRiver) {
  for (season in 1:4){
    K[,,season,river] <- Gxy[,,season,river]*Sx[,,season,river] + 
      F1x[,,season,river]*F2xy[,,season,river]*F3x[,,season,river]*SJx[,,season,river]
  }
}


#This creates the vec-permutation matrix 
perm <- array(0,c(length(y)*nRiver,length(y)*nRiver))
for (s in 1:length(y)){
  for (p in 1:nRiver){
    E <- array(0,c(length(y),nRiver))
    E[s,p] <- 1 
    Et <- t(E)
    Ek <- E %x% Et
    perm  <- perm + Ek
  }
}

# perm <- array(0,c(4,4))
# for (s in 1:2){
#   for (p in 1:2){
#     E <- array(0,c(2,2))
#     E[s,p] <- 1 
#     Et <- t(E)
#     Ek <- E %x% Et
#     perm  <- perm + Ek
#   }
# }




#This creates the metaK, which is a block diagonal with rivers in each block on the diagonal. 
#There should be a more versatile way to do this.If there are more rivers. But this is what I have for now

metaKrow4 <- metaKrow3 <-metaKrow2 <-metaKrow1 <- array(0,c(4,length(y),length(y)*nRiver))
metaK <- array(0,c(4,length(y)*nRiver,length(y)*nRiver))  #This is the whole metaK for a season 


zeros <- array(0,c(length(y),length(y)))

for (season in 1:4){
  metaKrow1[season,,] <- abind(K[,,season,1],zeros,zeros,zeros, along=2)
  metaKrow2[season,,] <- abind(zeros,K[,,season,2],zeros,zeros, along=2)
  metaKrow3[season,,] <- abind(zeros,zeros,K[,,season,3],zeros, along=2)
  metaKrow4[season,,] <- abind(zeros,zeros,zeros,K[,,season,4], along=2)
  
  metaK[season,,] <- abind(metaKrow1[season,,],metaKrow2[season,,],metaKrow3[season,,],metaKrow4[season,,],along=1)
  
}


##MetaM is the block diagonal matrix with migration. Movement probabilities for Stages in each block on the diagonal. 
#Within each block is the probabilities for stage i to move from river j to k  
metaM <- array(0,c(4,length(y)*nRiver,length(y)*nRiver)) 
for (season in 1:4){
  metaM[ season,, ] <- diag(1,length(y)) %x% psi[ season,, ]   #this will have to change if psi is size-specific
}


##loop through different starting seasons to get yearly KMyear matrices with different starting seasons
metaKMyear <- array(0,c(4,length(y)*nRiver,length(y)*nRiver))

for (firstSeason in 1:4){                         # firstSeason is the season in question and is the last season in the periodic matrix
  f <- c( firstSeason, firstSeason : (firstSeason+2) %% 4 + 1 )
  metaKMyear[firstSeason,,] <-  t(perm) %*% metaM[f[4],,] %*% perm %*% metaK[f[4],,] %*%
    t(perm) %*% metaM[f[3],,] %*% perm %*% metaK[f[3],,] %*%
    t(perm) %*% metaM[f[2],,] %*% perm %*% metaK[f[2],,] %*%
    t(perm) %*% metaM[f[1],,] %*% perm %*% metaK[f[1],,]
  
}


#This calculates the yearly lambdas, w and v in metaKMyear or the combined effect of demography and migration
#Note that the interpretation of these values are not straightforward, since v and w will be cyclical, Caswell p351-352.

vectorsmetaKMyear <- array(NA,c(2,4,length(y)*nRiver));   #w or v, season started with, length*nRiver, 
valuesmetaKMyear <- array(NA,c(4))                       
for(s in 1:4){
  vectorsmetaKMyear[1,s,] <- getEig(metaKMyear[s,,],F)
  vectorsmetaKMyear[2,s,] <- getEig(metaKMyear[s,,],T)
  valuesmetaKMyear[s] <- max(Re(eigen((metaKMyear[s,,]))$values))
}

valuesmetaKMyear  #These should all be the same
