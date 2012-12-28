

#This creates the function 'getEig', which is used to calculate w and v analytically. 
getEig <- function(mat,transpose){
  
#  vec <- Re(eigen(mat)$vectors[,1])                        #This calculates the right eigenvector 
#  if(transpose==T) vec <- Re(eigen(t(mat))$vectors[,1])   #This calculates the left eigenvector 
  
#  vec <- vec/(2*range(vec)[2]);                          #This standardizes the values by the largest value

  if(transpose==F) {vec=Re(eigen(mat)$vectors[,1]); vec=vec/sum(vec)}; 
  if(transpose==T) {vec=Re(eigen(t(mat))$vectors[,1]); vec=vec/sum(vec) };   #Note that this is not rescaled
  
  return(vec)
}



#This calculates the yearly lambdas, w and v in metaKMyear or the combined effect of demography and migration
#Note that the interpretation of w and v values are not straightforward, since v and w will be cyclical, Caswell p351-352.
#Lambda will not be affected since it is a yearly rate.




vectorsmetaKMyear <- array(NA,c(2,4,(length(y)+1)*nRiver));   #w or v, season started with, length*nRiver, 
valuesmetaKMyear <- array(NA,c(4))                       
for(season in 1:4){
  vectorsmetaKMyear[1,season,] <- getEig(metaKMyear[season,,],transpose=F)
  vectorsmetaKMyear[2,season,] <- getEig(metaKMyear[season,,],transpose=T)
  valuesmetaKMyear[season] <- max(Re(eigen((metaKMyear[season,,]))$values))

 
}






