setwd("C:\\Documents\\GitHub\\testing\\somepath2\\runs")
flow_file='.\\14\\seasonal_streamflow.csv';

temp_file='.\\15\\seasonal_streamtemp.csv';
# setwd(".\\16")

# read.csv(temp_file)

library(dodo); 
data('life_cycle-TSD');
pop$run(n=100, e=cbind(read.csv(flow_file),read.csv(temp_file)), o='C:\\Documents\\GitHub\\testing\\somepath2\\runs\\16')
2
4
