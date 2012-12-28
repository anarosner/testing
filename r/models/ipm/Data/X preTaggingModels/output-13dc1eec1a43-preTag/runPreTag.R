
rDataName <- 'dMDataOutBKT2002_2011.RData'

##########

home <- paste("~/Projects/Current/Westbrook/Brook Trout/Data/Estimation Models/preTaggingModels",sep='')
#home <- paste("/home/ron/allSpp/preTaggingModels",sep='')

setwd( home )
directory <- tempfile( pattern="output-", tmpdir ='.', fileext='-preTag')
dir.create(directory)

bugsName <- paste('./preTaggingLengthModel','.bugs',sep='')

file.copy(from='./callPreTagLength.R', to=paste(directory,'callPreTagLength.R',sep='/'))
file.copy(from=bugsName , to=paste(directory,bugsName ,sep='/'))
#file.copy(from='./analyzeMSRiver.R', to=paste(directory,'analyzeMSRiver.R',sep='/'))
file.copy(from='./runPreTag.R', to=paste(directory,'runPreTag.R',sep='/'))
##################
file.copy(from=paste(rDataName,sep=''), to=paste(directory,rDataName,sep='/'))
##################

setwd(directory)

fileOutName <- "outPreTagLength.RData" 

#load(paste(home,'/',rDataName,simNum,'.RData',sep=''))

####################
load(paste('~/Projects/Current/Westbrook/Brook Trout/Data/Estimation Models',rDataName,sep='/'))
load('~/Projects/Current/Westbrook/Brook Trout/Data/Estimation Models/earlyLifeInputForBen.RData')
#load(paste('/home/ron/allSpp',rDataName,sep='/'))
#load('/home/ron/allSpp/earlyLifeInputForBen.RData')
####################
#print(parms)

writeLines(text = '')
cat("Working directory for this run is: ", directory, "\n", sep = '')
cat("Output filename for this run is: ", fileOutName, "\n", sep = '')
writeLines(text = '')

source('./callPreTagLength.R')
save(dPreTag, outPre, file = fileOutName)

#writeLines(text=paste(date(),directory,afterAdapt - beforeAdapt,done - beforeJags), con='../latest_directory')
writeLines(text=paste(date(),directory,afterAdapt - beforeAdapt,done - beforeJags,"[", comment,"]"), con='./info.txt')
getwd()



################################################################
