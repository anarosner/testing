#rm(list=ls(all.names=TRUE))

# Ana's variables:
# run_dir <- '/tmp'
in_file <- 'o_table.csv'
out_file <- 'graphC.csv'
# End Ana's variables

o_table <- read.csv(
	file = file.path(run_dir, in_file), 
	check.names = FALSE,
	stringsAsFactors = FALSE
)

densities <- grepl(x=colnames(o_table), pattern='^\\d+', perl=TRUE)
smolt_stages <- c('two_spring_riverine', 'three_spring_riverine', 'four_spring_riverine')
graphC <- o_table[
	o_table[['season']] == 2 & 
	o_table[['stage_name']] %in% smolt_stages,
	c('stage_name','iteration','year',colnames(o_table)[densities])
]

stage_names <- graphC$stage_name

graphC <- graphC[,3:ncol(graphC)]
	
graphC <- split(x=graphC, f=stage_names)
graphC <- lapply(X=graphC, FUN=t)
graphC <- lapply(
	X=graphC, 
	FUN=function(x) {
		colnames(x) <- x[1,]
		x <- x[2:nrow(x),,drop=FALSE]
		return(x)
	}
)
silent <- mapply(
	FUN			=	write.csv, 
	x				=	graphC, 
	file		=	file.path(run_dir, paste(names(graphC), out_file, sep='-')  )
)
