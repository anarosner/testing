
iteration=function(Nt,season,transpose=F) {
  for (season in 1:4){                         # season is the season in question and is the last season in the periodic matrix
    f <- c( season, season : (season+2) %% 4 + 1 )
    Nt1 <- metaKMyear[season,,]%*%Nt
    if(transpose==T)    Nt1 <-t(metaKMyear[season,,])%*%Nt
    return(Nt1)
  }
}

#=========================================================================================#
#V) Start using the model 
#=========================================================================================#

# Estimate lambda and w or lambda and v (trans=T) by iterating unperturbed matrix 
#This calculates lambda, w and v by iteration. These are yearly lambdas, which should be same regardless of which 
#season is included first. w and v will depend on which season is included last in the periodic projection kernel
#Migration between streams can be turned on or off by move=T or move=F in the iteration function

vectorsmetaKMyear <- array(0,c(2,4,length(y)*nRiver))
valuesmetaKMyear <- array(1,c(4))
Nt <- Nt1 <- array(1,c(length(y)*nRiver))
riverlam <- array(0,c(4,nRiver))

for (trans in c(F,T)){
  #trans  <- T if want reprod vaule, held in index 2 in vectorStable and lamStable 
  #trans  <- F if want stable stage, held in index 1 in vectorStable and lamStable [default]
  #trans <- F
  
  for (season in 1:4){
    
    qmax=1000   #array(1000,c(4,nRiver)); 
    metalam <- metalamHold <- array(0,c(1))
    tol=1.e-10; 
    
    while(max(qmax)>tol) {
      
      Nt1 = iteration(Nt,season,transpose=trans)
        
      metalam <- sum(Nt1)/sum(Nt)  
      for (river in 1:nRiver){
        riverlam[season,river] <- sum(Nt1[(river*length(y)-99):(river*length(y))])/sum(Nt[(river*length(y)-99):(river*length(y))])
      }
      
      Nt <- Nt1#/lam
      
      qmax <- sum(abs(metalam-metalamHold))
      metalamHold <- metalam
      cat(season,metalam,qmax,"\n")
    }
    if ( trans == F ) vectorsmetaKMyear[1,season,]=Nt/sum(Nt); valuesmetaKMyear[season]=metalam
    if ( trans == T ) vectorsmetaKMyear[2,season,]=Nt/sum(Nt); 
  }
}