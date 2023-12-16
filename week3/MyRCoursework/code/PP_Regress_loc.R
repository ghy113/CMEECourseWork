# Load necessary libraries
library(plyr)

# Load the data
mydata <- read.csv("../data/EcolArchives-E089-51-D1.csv")

# Convert mg values to g
mydata$Prey.mass[mydata$Prey.mass.unit == "mg"] <- mydata$Prey.mass[mydata$Prey.mass.unit == "mg"] / 1000

# Define a function for linear regression analysis
lm_function <- function(data) {
  lm_result <- summary(lm(Prey.mass ~ Predator.mass, data = data))
  return(data.frame(
    slope = lm_result$coefficients[2],
    intercept = lm_result$coefficients[1],
    r2 = lm_result$r.squared,
    pvalue = lm_result$coefficients[8],
    fstat = lm_result$fstatistic[1]
  ))
}

# Apply linear regression function to subsets of the data
output <- ddply(mydata, .(Type.of.feeding.interaction, Predator.lifestage, Location), lm_function)

# Write the results to a CSV file
write.csv(output, "../results/PP_Regress_loc_Results.csv", row.names = FALSE)
