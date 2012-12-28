
iteration=function(Nt,season,transpose=F) {
#  for (season in 1:4){                         # season is the season in question and is the last season in the periodic matrix
#    f <- c( season, season : (season+2) %% 4 + 1 )
    Nt1 <- metaKMyear[season,,]%*%Nt
    if(transpose==T)    Nt1 <-t(metaKMyear[season,,])%*%Nt
    return(Nt1)
#  }
}

#=========================================================================================#
#V) Start using the model 
#=========================================================================================#

# Estimate lambda and w or lambda and v (trans=T) by iterating unperturbed matrix 
#This calculates lambda, w and v by iteration. These are yearly lambdas, which should be same regardless of which 
#season is included first. w and v will depend on which season is included last in the periodic projection kernel
#Migration between streams can be turned on or off by move=T or move=F in the iteration function

vectorsmetaKMyear <- array(0,c(2,4,(length(y)+1)*nRiver))
vectorsmetaKMyearGraph <- array(0,c(2,4,(length(y)+1)*nRiver))
valuesmetaKMyear <- array(1,c(4))
Nt <- Nt1 <- array(100,c((length(y)+1)*nRiver))
riverlam <- array(0,c(4,nRiver))

for (trans in c(F,T)){
  #trans  <- T if want reprod vaule, held in index 2 in vectorStable and lamStable 
  #trans  <- F if want stable stage, held in index 1 in vectorStable and lamStable [default]
  #trans <- F
  
  for (season in 1:4){
    
    qmax=1000   #array(1000,c(4,nRiver)); 
    metalam <- metalamHold <- array(0,c(1))
    tol=1.e-8; 
    
    while(max(qmax)>tol) {
      
      Nt1 = iteration(Nt,season,transpose=trans)
        
      metalam <- sum(Nt1)/sum(Nt)  
      for (river in 1:nRiver){
        riverlam[season,river] <- sum(Nt1[(river*(length(y)+1)-100):(river*(length(y)+1))])/sum(Nt[(river*(length(y)+1)-100):(river*(length(y)+1))])
      }
      
      Nt <- Nt1#/lam
      
      qmax <- sum(abs(metalam-metalamHold))
      metalamHold <- metalam
      cat(season,metalam,qmax,"\n")
    }
    for (river in 1:nRiver){
      
    if ( trans == F ) vectorsmetaKMyear[1,season,(river*(length(y)+1)-100):(river*(length(y)+1))]=Nt[(river*(length(y)+1)-100):(river*(length(y)+1))]/sum(Nt); 
    if ( trans == T ) vectorsmetaKMyear[2,season,(river*(length(y)+1)-100):(river*(length(y)+1))]=Nt[(river*(length(y)+1)-100):(river*(length(y)+1))]/sum(Nt);    
 
    if ( trans == F ) vectorsmetaKMyearGraph[1,season,(river*(length(y)+1)-100):(river*(length(y)+1))]=Nt[(river*(length(y)+1)-100):(river*(length(y)+1))]/sum(Nt[(river*(length(y)+1)-100):(river*(length(y)+1))]); 
    if ( trans == T ) vectorsmetaKMyearGraph[2,season,(river*(length(y)+1)-100):(river*(length(y)+1))]=Nt[(river*(length(y)+1)-100):(river*(length(y)+1))]/sum(Nt[(river*(length(y)+1)-100):(river*(length(y)+1))]);
    
    }
    valuesmetaKMyear[season]=metalam
    }
}