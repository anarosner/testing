
nIter <- length(out$grBeta[1,1,1,,1])

pList <- 
  within(
    data = list(),
    expr = {
      
      #     grBeta = aaply( out$grBeta[,,2:5,,], .margins=c(1,2,3), mean )
      #     phiBeta = aaply( out$phiBeta[,,2:5,,], .margins=c(1,2,3), mean )
      #psiBeta = aaply( invlogit( out$psiBeta ), .margins=c(1,2,3), mean )
      #     
      #     for ( i in 1:d$nRivers ){
      #       psiBeta[ i,i ]  <- 1 - sum( psiBeta[i,]) + psiBeta[ i,i ]
      #     }
      #        
      grBeta = ( out$grBeta[,,2:5,,] )
      phiBeta = ( out$phiBeta[,,2:5,,] )
      psiBeta = ( invlogit( out$psiBeta ) )
      
      for ( iter in 1:nIter ) { 
          for( s in 1:4 ) {
       for ( i in 1:d$nRivers ){
         psiBeta[ s,i,i,iter, ]  <- 1 - sum( psiBeta[s,i,,iter,]) + psiBeta[ s,i,i,iter, ]
       }  
      }
      }  
#       for( s in 1:4 ) {
#         for ( i in 1:d$nRivers ){
#           psiBeta[ s,i,i ]  <- 1 - sum( psiBeta[s,i,]) + psiBeta[ s,i,i ]
#         }
#       }
      
      #    grSigma= aaply( out$grSigma[,2:5,,], .margins=c(1,2), mean )
#      grSigmaBeta= ( out$grSigmaBeta[,,2:5,,] )
  
      grSigmaBeta= ( out$grSigma[,2:5,,,] )
      
#       kidSizeMeanInt = outPre$meanInt[,3801:4000,1]
#       kidSizeMeanBeta = outPre$meanBeta[1:16,,3801:4000,1]
#       kidSizeSdInt = outPre$sigmaInt[,3801:4000,1]
#       kidSizeSdBeta = outPre$sigmaBeta[1:16,,3801:4000,1]
#      
      
      kidSizeMeanBeta = outPre
      kidSizeSdInt = 4
      
      
      lengthMean = d$lengthMean[,2:5]
      lengthSd = d$lengthSd[,2:5]
      
      #rCutoff = 80,3
      rInt = probReproInt
      rSlope = probReproSlope
      
      #will need to rerun d from callMSRiver
#      kidSizeMean = d$lengthMean0 #indexed 1:4
#      kidSizeSd = d$lengthSd0 #indexed 1:4
      nKidsSizeInt = 0.00187 
      NKidsSizeSlope = 2.19
      
#       earlySurv = (preRecruitSurvMain)^(1/4) #per season prefish survival
#       earlySurvObear = (preRecruitSurvIso)^(1/4)
      earlySurvBeta <- outSj
      
      nRiver <- d$nRiver
      #  etc.
    }
    )




