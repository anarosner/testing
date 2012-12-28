pList <- 
  within(
    data = list(),
    expr = {
      
      #     grBeta = aaply( out$grBeta[,,2:5,,], .margins=c(1,2,3), mean )
      #     phiBeta = aaply( out$phiBeta[,,2:5,,], .margins=c(1,2,3), mean )
      psiBeta = aaply( invlogit( out$psiBeta ), .margins=c(1,2,3), mean )
      #     
      #     for ( i in 1:d$nRivers ){
      #       psiBeta[ i,i ]  <- 1 - sum( psiBeta[i,]) + psiBeta[ i,i ]
      #     }
      #        
      grBeta = ( out$grBeta[,,2:5,,] )
      phiBeta = ( out$phiBeta[,,2:5,,] )
      #psiBeta = ( invlogit( out$psiBeta ) )
      
      #for ( iter in 1:nIter ) { 
      #     for( s in 1:4 ) {
      #  for ( i in 1:d$nRivers ){
      #    psiBeta[ s,i,i,iter, ]  <- 1 - sum( psiBeta[s,i,,iter,]) + psiBeta[ s,i,i,iter, ]
      #  }  
      # }
      #}  
      for( s in 1:4 ) {
        for ( i in 1:d$nRivers ){
          psiBeta[ s,i,i ]  <- 1 - sum( psiBeta[s,i,]) + psiBeta[ s,i,i ]
        }
      }
      
      #    grSigma= aaply( out$grSigma[,2:5,,], .margins=c(1,2), mean )
      grSigma= ( out$grSigma[,2:5,,] )
      
      
      lengthMean = d$lengthMean[,2:5]
      lengthSd = d$lengthSd[,2:5]
      
      #rCutoff = 80
      rInt = 80
      rSlope = 3
      
      #will need to rerun d from callMSRiver
      kidSizeMean = d$lengthMean0 #indexed 1:4
      kidSizeSd = d$lengthSd0 #indexed 1:4
      nKidsSizeInt = 0.00187 
      NKidsSizeSlope = 2.19
      earlySurv = 0.03356^(1/3.5) #per season prefish survival
      earlySurvObear = (0.03356*1.53)^(1/3.5)
      nRiver <- d$nRiver
      #  etc.
    }
    )






nIter <- 200

# boundary points b and mesh points y
b = L+c(0:n)*(U-L)/n; 
y = 0.5*(b[1:n]+b[2:(n+1)]);

# step size for midpoint rule, see equations 4 and 5
h = y[2]-y[1]

ylong <- array(0,c(length(y)+1,nRiver))
for(river in 1:nRiver){
  ylong[2:(length(y)+1),river] <- y
}
ylong <- c(as.matrix(ylong))