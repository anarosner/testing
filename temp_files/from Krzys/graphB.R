rm(list=ls(all.names=TRUE))

# Ana's variables:
run_dir <- '/tmp'
in_file <- 'o_table.csv'
out_file <- 'graphB.csv'
n_bins <- 5
keeper_columns <- c('year','total')
# End Ana's variables

o_table <- read.csv(
	file = file.path(run_dir, in_file), 
	check.names = FALSE,
	stringsAsFactors = FALSE
)

graphB <- o_table[
	o_table[['season']] == 3,
]

graphB <- split(x=graphB, f=graphB$stage_name)

graphB <- lapply(
	X = graphB,
	FUN = function(dat) {
		nrd <- nrow(dat)
		densities <- grepl(x=colnames(dat), pattern='^\\d+', perl=TRUE)
		header <- dat[,!densities, drop=FALSE]
		d <- as.matrix(dat[,densities, drop=FALSE])
		midpoints <- as.numeric(colnames(d))
		rs <- apply(X=d, MARGIN=1, FUN=sum)
		frac_of_rs <- apply(X=d, MARGIN=2, FUN=function(x) x/rs) 
		if (nrd == 1) frac_of_rs <- as.matrix(frac_of_rs, nrow=1)
		max_frac_of_rs <- apply(X=frac_of_rs, MARGIN=2, FUN=max)
		keepers <- max_frac_of_rs > 0.001
		d <- d[,keepers, drop=FALSE]
		midpoints <- midpoints[keepers]
		if (nrd == 1) d <- matrix(data=d, nrow=1)

		## Calculate breakpoints for this particular stage, and their
		## labels:
		groups <- cut(x=midpoints, breaks=n_bins-1)

		d <- apply(
			X = d,
			MARGIN = 1,
			FUN = function(x, groups) {
###				groups <- cut(x=midpoints, breaks=n_bins-1)
				x <- aggregate(x=x, by = list(groups = groups), FUN = sum)$x
###				names(x) <- levels(groups)
				return(x)
			},
			groups = groups
		)
		rownames(d) <- levels(groups)
		d <- t(d)
		complete <- cbind(header, d)
		return(complete[,c(keeper_columns, levels(groups)), drop=FALSE])
	}
)


silent <- mapply(
	FUN 		= write.csv, 
	x				= graphB,
	file		= file.path(run_dir, paste(names(graphB),out_file,sep='-') )
)
