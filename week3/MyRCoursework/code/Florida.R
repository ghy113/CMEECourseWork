rm(list=ls())
load("../data/KeyWestAnnualMeanTemperature.RData")

ls()
class(ats)
head(ats)

png("01_time.png")
plot(ats$Year, ats$Temp, xlab="Year", ylab="Temperature", main="Annual Mean Temperature in Key West, Florida (20th Century)", type="l")
dev.off()

observed_cor <- cor(ats$Year, ats$Temp)


# Set the number of permutations
n_iterations <- 10000

# Store correlation coefficients from each iteration
random_cor <- numeric(n_iterations)

# Permutation test
for (i in 1:n_iterations) {
  shuffled_temp <- sample(ats$Temp)
  random_cor[i] <- cor(ats$Year, shuffled_temp)
}

# Plot histogram of random correlation coefficients
png("01_corr.png")
hist(random_cor, breaks=30,xlim=range(c(random_cor, observed_cor)), main="Distribution of Random Correlation Coefficients", xlab="Correlation Coefficient", col="skyblue", border="black")
abline(v=observed_cor, col="red", lwd=2)
legend("topright", legend=c("Observed Correlation"), col=c("red"), lwd=2)
dev.off()


p_value <- sum(random_cor > observed_cor) / n_iterations
cat("p_value: ", p_value, "\n");