




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



#============================================================================================#
# (III) Here is the main model for asymptotic values
#============================================================================================#




#This calculates the yearly lambdas, w and v in metaKMyear or the combined effect of demography and migration
#Note that the interpretation of these values are not straightforward, since v and w will be cyclical, Caswell p351-352.

vectorsmetaKMyear <- array(NA,c(2,4,length(y)*nRiver));   #w or v, season started with, length*nRiver, 
valuesmetaKMyear <- array(NA,c(4))                       
for(s in 1:4){
  vectorsmetaKMyear[1,s,] <- getEig(metaKMyear[s,,],F)
  vectorsmetaKMyear[2,s,] <- getEig(metaKMyear[s,,],T)
  valuesmetaKMyear[s] <- max(Re(eigen((metaKMyear[s,,]))$values))

valuesmetaKMyear[s]  #These should all be the same


}

