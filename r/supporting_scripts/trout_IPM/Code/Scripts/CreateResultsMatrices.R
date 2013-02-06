if (zeroCond==F){
  metaLambdas <- array(NA,c(npop,nRiver,nSeason,length(fvariables),length(tvariables)))
  envsens <- array(NA,c(npop,nRiver,2,2,4,length(fvariables),length(tvariables)))
}

if (zeroCond==T){
  metaLambdas <- array(NA,c(npop))
}


if (zeroCond==T){
  if (splitStreams==T){
    riverLamdasmain <- array(NA,c(nRiver))
    riverLamdasIso <- array(NA,c((npop-1),nRiver))
  }
  
}

if (zeroCond==F){
  if (splitStreams==T){ 
    riverLamdasmain <- array(NA,c(nRiver,nSeason,length(fvariables),length(tvariables),nRiver))
    riverLamdasIso <- array(NA,c((npop-1),nRiver,nSeason,length(fvariables),length(tvariables),nRiver))
  }
  riverLamdasmain <- array(NA,c(nRiver,nSeason,length(fvariables),length(tvariables),nRiver))
}


if (splitStreams==T){
  if (zeroCond==T){
    MSensIso <- array(NA,c(npop,4,(length(y)+1),(length(y)+1))) 
    MElasIso <- array(NA,c(npop,4,(length(y)+1),(length(y)+1)))
    KSensIso <- array(NA,c(npop,4,(length(y)+1),(length(y)+1))) 
    KElasIso <- array(NA,c(npop,4,(length(y)+1),(length(y)+1)))
    
    vitalSensIso <- array(NA,c(npop,6,4,(length(y)+1),(length(y)+1)))
    vitalElasIso <- array(NA,c(npop,6,4,(length(y)+1),(length(y)+1)))
  }
}


