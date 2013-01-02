

win.graph(); par(mfrow=c(4,4));   #River across the top, season along the side
persp(y,y,pDemoSensSeason[rate,1,1:100,1:100], theta=60, zlim=c(lol,upl), ticktype="detailed",r=10,
      xlab="size at time t+1", ylab="size at time t",zlab="Sensitivity Spring River 1"); 
persp(y,y,pDemoSensSeason[rate,1,101:200,101:200], theta=60, zlim=c(lol,upl),ticktype="detailed",r=10,
      xlab="size at time t+1", ylab="size at time t",zlab="Sensitivity Spring River 2");
persp(y,y,pDemoSensSeason[rate,1,201:300,201:300], theta=60, zlim=c(lol,upl),ticktype="detailed",r=10,
      xlab="size at time t+1", ylab="size at time t",zlab="Sensitivity Spring River 3");
persp(y,y,pDemoSensSeason[rate,1,301:400,301:400], theta=60, zlim=c(lol,upl),ticktype="detailed",r=10,
      xlab="size at time t+1", ylab="size at time t",zlab="Sensitivity Spring River 4");

persp(y,y,pDemoSensSeason[rate,2,1:100,1:100], theta=60, zlim=c(lol,upl), ticktype="detailed",r=10,
      xlab="size at time t+1", ylab="size at time t",zlab="Sensitivity Summer River 1"); 
persp(y,y,pDemoSensSeason[rate,2,101:200,101:200], theta=60, zlim=c(lol,upl),ticktype="detailed",r=10,
      xlab="size at time t+1", ylab="size at time t",zlab="Sensitivity Summer River 2");
persp(y,y,pDemoSensSeason[rate,2,201:300,201:300], theta=60, zlim=c(lol,upl),ticktype="detailed",r=10,
      xlab="size at time t+1", ylab="size at time t",zlab="Sensitivity Summer River 3");
persp(y,y,pDemoSensSeason[rate,2,301:400,301:400], theta=60, zlim=c(lol,upl),ticktype="detailed",r=10,
      xlab="size at time t+1", ylab="size at time t",zlab="Sensitivity Summer River 4");

persp(y,y,pDemoSensSeason[rate,3,1:100,1:100], theta=60, zlim=c(lol,upl), ticktype="detailed",r=10,
      xlab="size at time t+1", ylab="size at time t",zlab="Sensitivity Fall River 1"); 
persp(y,y,pDemoSensSeason[rate,3,101:200,101:200], theta=60, zlim=c(lol,upl),ticktype="detailed",r=10,
      xlab="size at time t+1", ylab="size at time t",zlab="Sensitivity Fall River 2");
persp(y,y,pDemoSensSeason[rate,3,201:300,201:300], theta=60, zlim=c(lol,upl),ticktype="detailed",r=10,
      xlab="size at time t+1", ylab="size at time t",zlab="Sensitivity Fall River 3");
persp(y,y,pDemoSensSeason[rate,3,301:400,301:400], theta=60, zlim=c(lol,upl),ticktype="detailed",r=10,
      xlab="size at time t+1", ylab="size at time t",zlab="Sensitivity Fall River 4");

persp(y,y,pDemoSensSeason[rate,4,1:100,1:100], theta=60, zlim=c(lol,upl), ticktype="detailed",r=10,
      xlab="size at time t+1", ylab="size at time t",zlab="Sensitivity Winter River 1"); 
persp(y,y,pDemoSensSeason[rate,4,101:200,101:200], theta=60, zlim=c(lol,upl),ticktype="detailed",r=10,
      xlab="size at time t+1", ylab="size at time t",zlab="Sensitivity Winter River 2");
persp(y,y,pDemoSensSeason[rate,4,201:300,201:300], theta=60, zlim=c(lol,upl),ticktype="detailed",r=10,
      xlab="size at time t+1", ylab="size at time t",zlab="Sensitivity Winter River 3");
persp(y,y,pDemoSensSeason[rate,4,301:400,301:400], theta=60, zlim=c(lol,upl),ticktype="detailed",r=10,
      xlab="size at time t+1", ylab="size at time t",zlab="Sensitivity Winter River 4");

