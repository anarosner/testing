rm(list=ls(all.names=TRUE))

# Ana's variables:
# run_dir <- '/tmp'
in_file <- 'o_table.csv'
out_file <- 'graphA.csv'
# End Ana's variables:

o_table <- read.csv(
	file = file.path(run_dir, in_file), 
	check.names = FALSE,
	stringsAsFactors = FALSE
)

smolt_stages <- c('two_spring_riverine', 'three_spring_riverine', 'four_spring_riverine')
graphA <- o_table[
	o_table[['season']] == 2 & 
	o_table[['stage_name']] %in% smolt_stages,
	c('stage_name','iteration','year','season','total')
]

silent <- write.csv(x=graphA, file=file.path(run_dir, out_file))



