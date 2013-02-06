rm(list=ls(all.names=TRUE))

# Ana's variables:
# run_dir <- '/tmp'
in_file <- 'o_table.csv'
out_file <- 'graphD.csv'
plotting_stages <- c('zero_autumn_parr', 'one_autumn_parr','two_autumn_parr','three_autumn_parr')
# End Ana's variables

o_table <- read.csv(
	file = file.path(run_dir, in_file), 
	check.names = FALSE,
	stringsAsFactors = FALSE
)

densities <- grepl(x=colnames(o_table), pattern='^\\d+', perl=TRUE)
graphD <- o_table[
	o_table[['season']] == 3 & 
	o_table[['stage_name']] %in% plotting_stages,
	c('stage_name','iteration','year',colnames(o_table)[densities])
]

stage_names <- graphD$stage_name

graphD <- graphD[,3:ncol(graphD)]
	
graphD <- split(x=graphD, f=stage_names)
graphD <- lapply(X=graphD, FUN=t)
graphD <- lapply(
	X=graphD, 
	FUN=function(x) {
		colnames(x) <- x[1,]
		x <- x[2:nrow(x),,drop=FALSE]
		return(x)
	}
)
silent <- mapply(
	FUN		=	write.csv, 
	x			=	graphD, 
	file	=	file.path(run_dir, paste(names(graphD), out_file, sep='-')  )
)
