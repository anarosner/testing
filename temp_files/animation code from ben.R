library(ggplot2)
library(fMultivar)
library(animation)
library(grid)

setwd("C:/Documents/GitHub/testing/temp_files/from Ben")
load("gcmData.RData")
load("tempGCMData.RData")

xOffsetContour <- -1
yOffsetContour <- -1 
currentOffset <- 0.25

xGCMOffset <- seq( xOffsetContour+currentOffset,0.51,0.1 )
yGCMOffset <- seq( yOffsetContour+currentOffset,0.51,0.1 )

xGCMOffsetSD <- seq( 0.04,0.45,length.out=length(xGCMOffset) )
yGCMOffsetSD <- seq( 0.04,0.51,length.out=length(yGCMOffset) )


x = (-20:20)/10
X = grid2d(x)
z = dnorm2d(X$x-xOffsetContour, X$y-yOffsetContour, rho = -0.65)
#Z = list(x = x, y = x, z = matrix(z, ncol = length(x)))
#Z = data.frame(x = x, y = x, z = matrix(z, ncol = length(x)))
#persp(Z, theta = -40, phi = 30, col = "steelblue")
ggZ <- data.frame(cbind(x=X$x, y=X$y, z=z))

p <- ggplot(data=ggZ, aes(x=x,y=y)) + 
  geom_tile(aes(fill = z)) +
  stat_contour(data=ggZ,aes(z=z)) +
  stat_contour(data=ggZ,aes(z=z), bins=1.8, colour='white', size=2)
# in new version of ggplot, need to separate lines with different dimensions in the aes()

xSlope <- 0.2; ySlope <- 0.1
xInt <- xOffsetContour+currentOffset
yInt <- yOffsetContour+currentOffset
lineTrace <- data.frame(index = ( 0:( length(xGCMOffset) -1 ) ))
lineTrace$x = lineTrace$index * xSlope + xInt
lineTrace$y = lineTrace$index * ySlope + yInt

pExtinct <- data.frame( x=1:( length(xGCMOffset)) )
pExtinct$y <- pExtinct$x*0.1
################
# set viewport to insert second graph
vp <- viewport(width = 0.3, height = 0.3, x = 0.97, y = unit(0.9, "lines"), just = c("right","bottom"))

## set some options first
oopt = ani.options( interval = 0.8, nmax = length(xGCMOffset) )
## use a loop to create images one by one
saveHTML({
  
  for (i in 1:ani.options("nmax")) {
    print(i)
    gcm <- data.frame(xx=rnorm(100,xGCMOffset[i],xGCMOffsetSD[i]),yy=rnorm(100,yGCMOffset[i],yGCMOffsetSD[i]))
    
    print(
      p+
        geom_point(aes(xOffsetContour+currentOffset,yOffsetContour+currentOffset), size=20) +
        geom_point(aes(xOffsetContour+currentOffset,yOffsetContour+currentOffset), size=10, colour='white')+
        geom_point(data=gcm, aes(x=xx,y=yy), colour='yellow') +
        geom_point( data=lineTrace[1:i,], aes(x=x,y=y), colour='lightblue', size=3) +
        geom_line( data=lineTrace[1:i,], aes(x=x,y=y), colour='lightblue') +     
        scale_x_continuous('Standardized air temperature') +
        scale_y_continuous('Standardized precipitation')+
        ggtitle(i)
    )
    
    print(
      ggplot(pExtinct[1:i,], aes(x,y))+  
        geom_point()+
        scale_x_continuous(lim=c(0,ani.options("nmax")), 'Time') +
        scale_y_continuous(lim=c(0,1.1), 'Proportion of gcm > lambda=1')+
        theme(axis.title.x = element_text( size=10 ),
              axis.title.y = element_text( size=10 )),
      vp = vp
    ) 
    #theme_set(theme_bw())
    
    ani.pause() ## pause for a while ('interval')
  }
  
},
         img.name = "countourPlots", imgdir = "plots", htmlfile = "random1.html",
         autobrowse = TRUE,
         verbose=FALSE
)         