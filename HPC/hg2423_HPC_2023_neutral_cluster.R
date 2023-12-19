# CMEE 2022 HPC exercises R code pro forma
# For neutral model cluster run
# question 18
rm(list=ls()) # good practice 
graphics.off()
source("/rds/general/user/hg2423/home/ghy113/hg2423_HPC_2023_main.R")

# Open when code is submitted
iter <- as.numeric(Sys.getenv("PBS_ARRAY_INDEX"))
# iter <- 1

# fix seed
set.seed(iter)

# Select the corresponding size according to iter
if (1 <= iter && iter <= 25) {
    size <- 500
} else if (iter <= 50) {
    size <- 1000
} else if (iter <= 75) {
    size <- 2500
} else {
    size <- 5000
}
# My specification_rate
speciation_rate <- 0.004131 # hg2423
# Create a file name to save the results
output_file_name <- paste("neutral_cluster_", iter, ".rda", sep="")

neutral_cluster_run(speciation_rate, size, wall_time=11.5 * 60, 
interval_rich=1, interval_oct=size/10, burn_in_generations=8 * size, 
output_file_name)