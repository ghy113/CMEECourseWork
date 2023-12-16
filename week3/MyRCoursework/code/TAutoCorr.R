rm(list=ls())

load("../data/KeyWestAnnualMeanTemperature.RData")

# Set seed for reproducibility 
set.seed(123)

# Calculate the correlation coefficient between consecutive years
original_corr <- cor(ats$Temp[-length(ats$Temp)], ats$Temp[-1])

# Set the number of permutations
n_permutations <- 10000

# Initialize a vector to store the correlation coefficient after permutation
permuted_corrs <- numeric(n_permutations)

# Perform permutation and calculate correlation coefficients
for(i in 1:n_permutations){
  permuted_temp <- sample(ats$Temp)
  permuted_corrs[i] <- cor(permuted_temp[-length(permuted_temp)], permuted_temp[-1])
}

# p-value
p_value <- sum(permuted_corrs > original_corr) / n_permutations

# Draw raw temperature data graph
png("02_time.png")
plot(ats$Year, ats$Temp, type='l', main="Key West Annual Mean Temperature", xlab="Year", ylab=expression("Temperature ("*degree*C*")"))
dev.off()

# Draw a correlation coefficient distribution map after permutation
png("02_corr.png")
hist(permuted_corrs, breaks=50, main="Distribution of Correlation Coefficients from Permutations", xlab="Correlation Coefficient")
abline(v=original_corr, col="red")
dev.off()


print(paste("Approximate p-value:", p_value))