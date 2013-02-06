library(dodo)
data('life_cycle-ATS-smolt')

# # Ana's variables:
# stocking_n <- 100
# flow_dir <- '~/downloads'
# streamtemp_dir <- '~/downloads'
# run_dir <- 'test'
out_file <- 'o_table.csv'
# # End Ana's variables

# Stocking
stocking_program <- pop$add(
  stage="stock",
  args=list(
    n_bins=350,
    limits=c(min=0, max=350),
    density=function(y) dnorm(x=y, mean=15, sd=1),
		N=stocking_n
  )
)

# Environment.
path_to_flow <- file.path(flow_dir, 			"seasonal_streamflow.csv")
path_to_temp <- file.path(streamtemp_dir, "seasonal_streamtemp.csv")
flow <- read.csv(path_to_flow)
temp <- read.csv(path_to_temp)
envir <- merge(x=flow, y=temp, by=c('year','season'))
envir[['season']] <- as.factor(envir[['season']])
envir[['iteration']] <- 1:nrow(envir)

# Run
#pop$run(n=4*4, e=envir, o=run_dir)

print(system.time( expr = {
	options(mc.cores=8)
	file.remove(dir(run_dir, full.names=TRUE))
	pop$run(n=nrow(envir), e=envir, o=run_dir)
}, gcFirst=TRUE))


print(system.time( expr = {
# Rewrite output 
o_files <- dir(path=run_dir, pattern='^pops-')
its <- get_regexpr(pattern='ITER:\\d+', text=o_files, offset = 5, perl=TRUE)

o_data <- list()
for (o_file in o_files) {
	i <- as.numeric(its[o_file])
	o_data[[i]] <- readRDS(file.path(run_dir,o_file))
	o_data[[i]] <-  do.call(what=rbind, args=lapply(o_data[[i]], as.data.frame))
	o_data[[i]][['iteration']] <- i
}

o_table <- do.call(what=rbind, args=o_data)
o_table <- merge(x=o_table, y=envir, by='iteration')
densities <- grepl(x=colnames(o_table), pattern='^\\d+', perl=TRUE)
total_counts <- apply(X=as.matrix(o_table[,densities]), MARGIN=1, FUN=sum )
o_table[['total']] <- total_counts
densities <- grepl(x=colnames(o_table), pattern='^\\d+', perl=TRUE)
o_table <- o_table[total_counts > (0.0001 * max(total_counts)),]

write.csv(x=o_table, file=file.path(run_dir,out_file))
})


