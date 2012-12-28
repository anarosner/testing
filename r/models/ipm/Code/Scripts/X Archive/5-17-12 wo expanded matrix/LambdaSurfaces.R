





perturblambdas <- array(NA,c(nRiver,4,length(fvariables),length(tvariables))) 

for (pertriver in 1:nRiver){
  for (firstSeason in 1:4){
    for (temppert in 1:(length(tvariables))){
     temp <- tvariables[temppert]
       for (flowpert in 1:(length(fvariables))){
         flow <- fvariables[flowpert]
            source("Scripts/Demographic Functions.R")
            source("Scripts/ConstCompMatrices.R")
            source("Scripts/MetaKM.R")
            source("Scripts/EigCalcAnalytical.R")
            perturblambdas[pertriver,firstSeason,flowpert,temppert] <- valuesmetaKMyear[1]             #firstSeason here tells us the lambda from perturbing variables in that season
      }
    }
  } 
}


win.graph(); par(mfrow=c(2,2));
persp(tvariables,fvariables,perturblambdas[1,1,,], theta=60, zlim=c(.95,1), ticktype ="detailed",
      xlab=" Seasonal Temperature", ylab=" Seasonal Flow",zlab="Lambda");title("River 1 Season 1");
persp(tvariables,fvariables,perturblambdas[1,2,,], theta=60, zlim=c(.95,.98), ticktype ="detailed",
      xlab=" Seasonal Temperature", ylab=" Seasonal Flow",zlab="Lambda");title("River 1 Season 2");
persp(tvariables,fvariables,perturblambdas[1,3,,], theta=60, zlim=c(.95,1.05), ticktype ="detailed",
      xlab=" Seasonal Temperature", ylab=" Seasonal Flow",zlab="Lambda");title("River 1 Season 3"); 
persp(tvariables,fvariables,perturblambdas[1,4,,], theta=60, zlim=c(.95,.98), ticktype ="detailed",
      xlab=" Seasonal Temperature", ylab=" Seasonal Flow",zlab="Lambda");title("River 1 Season 4"); 


win.graph(); par(mfrow=c(2,2));
persp(tvariables,fvariables,perturblambdas[2,1,,], theta=60, zlim=c(.96,.98), ticktype ="detailed",
      xlab=" Seasonal Temperature", ylab=" Seasonal Flow",zlab="Lambda");title("River 2 Season 1");
persp(tvariables,fvariables,perturblambdas[2,2,,], theta=60, zlim=c(.966,.971), ticktype ="detailed",
      xlab=" Seasonal Temperature", ylab=" Seasonal Flow",zlab="Lambda");title("River 2 Season 2");
persp(tvariables,fvariables,perturblambdas[2,3,,], theta=60, zlim=c(.95,.99), ticktype ="detailed",
      xlab=" Seasonal Temperature", ylab=" Seasonal Flow",zlab="Lambda");title("River 2 Season 3"); 
persp(tvariables,fvariables,perturblambdas[2,4,,], theta=60, zlim=c(.95,1), ticktype ="detailed",
      xlab=" Seasonal Temperature", ylab=" Seasonal Flow",zlab="Lambda");title("River 2 Season 4"); 


win.graph(); par(mfrow=c(2,2));
persp(tvariables,fvariables,perturblambdas[3,1,,], theta=60, zlim=c(.95,1.05), ticktype ="detailed",
      xlab=" Seasonal Temperature", ylab=" Seasonal Flow",zlab="Lambda");title("River 3 Season 1");
persp(tvariables,fvariables,perturblambdas[3,2,,], theta=60, zlim=c(.95,1.05), ticktype ="detailed",
      xlab=" Seasonal Temperature", ylab=" Seasonal Flow",zlab="Lambda");title("River 3 Season 2");
persp(tvariables,fvariables,perturblambdas[3,3,,], theta=60, zlim=c(.95,1.15), ticktype ="detailed",
      xlab=" Seasonal Temperature", ylab=" Seasonal Flow",zlab="Lambda");title("River 3 Season 3"); 
persp(tvariables,fvariables,perturblambdas[3,4,,], theta=60, zlim=c(.95,1), ticktype ="detailed",
      xlab=" Seasonal Temperature", ylab=" Seasonal Flow",zlab="Lambda");title("River 3 Season 4");

win.graph(); par(mfrow=c(2,2));
persp(tvariables,fvariables,perturblambdas[4,1,,], theta=60, zlim=c(.8,1.2), ticktype ="detailed",
      xlab=" Seasonal Temperature", ylab=" Seasonal Flow",zlab="Lambda");title("River 4 Season 1");
persp(tvariables,fvariables,perturblambdas[4,2,,], theta=60, zlim=c(.8,1.2), ticktype ="detailed",
      xlab=" Seasonal Temperature", ylab=" Seasonal Flow",zlab="Lambda");title("River 4 Season 2");
persp(tvariables,fvariables,perturblambdas[4,3,,], theta=60, zlim=c(.9,1.05), ticktype ="detailed",
      xlab=" Seasonal Temperature", ylab=" Seasonal Flow",zlab="Lambda");title("River 4 Season 3"); 
persp(tvariables,fvariables,perturblambdas[4,4,,], theta=60, zlim=c(.8,1.3), ticktype ="detailed",
      xlab=" Seasonal Temperature", ylab=" Seasonal Flow",zlab="Lambda");title("River 4 Season 4");
























win.graph(); par(mfrow=c(2,2));
persp(tvariables,fvariables,perturblambdas[1,1,,], theta=60, zlim=c(.95,1), ticktype ="detailed",
      xlab=" Seasonal Temperature", ylab=" Seasonal Flow",zlab="Lambda");title("River 1 Season 1");
persp(tvariables,fvariables,perturblambdas[2,1,,], theta=60, zlim=c(.96,.98), ticktype ="detailed",
      xlab=" Seasonal Temperature", ylab=" Seasonal Flow",zlab="Lambda");title("River 2 Season 1");
persp(tvariables,fvariables,perturblambdas[3,1,,], theta=60, zlim=c(.95,1.05), ticktype ="detailed",
      xlab=" Seasonal Temperature", ylab=" Seasonal Flow",zlab="Lambda");title("River 3 Season 1");
persp(tvariables,fvariables,perturblambdas[4,1,,], theta=60, zlim=c(.8,1.2), ticktype ="detailed",
      xlab=" Seasonal Temperature", ylab=" Seasonal Flow",zlab="Lambda");title("River 4 Season 1");



win.graph(); par(mfrow=c(2,2));
persp(tvariables,fvariables,perturblambdas[1,2,,], theta=60, zlim=c(.95,.98), ticktype ="detailed",
      xlab=" Seasonal Temperature", ylab=" Seasonal Flow",zlab="Lambda");title("River 1 Season 2");
persp(tvariables,fvariables,perturblambdas[2,2,,], theta=60, zlim=c(.966,.971), ticktype ="detailed",
      xlab=" Seasonal Temperature", ylab=" Seasonal Flow",zlab="Lambda");title("River 2 Season 2");
persp(tvariables,fvariables,perturblambdas[3,2,,], theta=60, zlim=c(.95,1.05), ticktype ="detailed",
      xlab=" Seasonal Temperature", ylab=" Seasonal Flow",zlab="Lambda");title("River 3 Season 2");
persp(tvariables,fvariables,perturblambdas[4,2,,], theta=60, zlim=c(.8,1.2), ticktype ="detailed",
      xlab=" Seasonal Temperature", ylab=" Seasonal Flow",zlab="Lambda");title("River 4 Season 2");



win.graph(); par(mfrow=c(2,2));
persp(tvariables,fvariables,perturblambdas[1,3,,], theta=60, zlim=c(.95,1.05), ticktype ="detailed",
      xlab=" Seasonal Temperature", ylab=" Seasonal Flow",zlab="Lambda");title("River 1 Season 3");
persp(tvariables,fvariables,perturblambdas[2,3,,], theta=60, zlim=c(.95,.99), ticktype ="detailed",
      xlab=" Seasonal Temperature", ylab=" Seasonal Flow",zlab="Lambda");title("River 2 Season 3");
persp(tvariables,fvariables,perturblambdas[3,3,,], theta=60, zlim=c(.95,1.15), ticktype ="detailed",
      xlab=" Seasonal Temperature", ylab=" Seasonal Flow",zlab="Lambda");title("River 3 Season 3"); 
persp(tvariables,fvariables,perturblambdas[4,3,,], theta=60, zlim=c(.9,1.05), ticktype ="detailed",
      xlab=" Seasonal Temperature", ylab=" Seasonal Flow",zlab="Lambda");title("River 4 Season 3");


win.graph(); par(mfrow=c(2,2));
persp(tvariables,fvariables,perturblambdas[1,4,,], theta=60, zlim=c(.95,.98), ticktype ="detailed",
      xlab=" Seasonal Temperature", ylab=" Seasonal Flow",zlab="Lambda");title("River 1 Season 4");
persp(tvariables,fvariables,perturblambdas[2,4,,], theta=60, zlim=c(.95,1), ticktype ="detailed",
      xlab=" Seasonal Temperature", ylab=" Seasonal Flow",zlab="Lambda");title("River 2 Season 4");
persp(tvariables,fvariables,perturblambdas[3,4,,], theta=60, zlim=c(.95,1), ticktype ="detailed",
      xlab=" Seasonal Temperature", ylab=" Seasonal Flow",zlab="Lambda");title("River 3 Season 4");
persp(tvariables,fvariables,perturblambdas[4,4,,], theta=60, zlim=c(.8,1.3), ticktype ="detailed",
      xlab=" Seasonal Temperature", ylab=" Seasonal Flow",zlab="Lambda");title("River 4 Season 4");
