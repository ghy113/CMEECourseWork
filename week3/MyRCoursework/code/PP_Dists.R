library(ggplot2)
library(tidyverse)

MyDF <- read.csv("../data/EcolArchives-E089-51-D1.csv")

# compute 
MyDF$log_predator_mass <- log(MyDF$Predator.mass)
MyDF$log_prey_mass <- log(MyDF$Prey.mass)
MyDF$log_size_ratio <- log(MyDF$Prey.mass/MyDF$Predator.mass)

# Plot
pdf("../results/Pred_Subplots.pdf")
par(mfrow=c(3, 2))
feeding_types <- unique(MyDF$Type.of.feeding.interaction)
for(feeding_type in feeding_types) {
  hist(subset(MyDF, Type.of.feeding.interaction == feeding_type)$log_predator_mass, 
       main=feeding_type, xlab="Log(Predator Mass)")
}
dev.off()

pdf("../results/Prey_Subplots.pdf")
par(mfrow=c(3, 2))
for(feeding_type in feeding_types) {
  hist(subset(MyDF, Type.of.feeding.interaction == feeding_type)$log_prey_mass, 
       main=feeding_type, xlab="Log(Prey Mass)")
}
dev.off()

pdf("../results/SizeRatio_Subplots.pdf")
par(mfrow=c(3, 2))
for(feeding_type in feeding_types) {
  hist(subset(MyDF, Type.of.feeding.interaction == feeding_type)$log_size_ratio, 
       main=feeding_type, xlab="Log(Size Ratio)")
}
dev.off()


results <- MyDF %>%
  group_by(Type.of.feeding.interaction) %>%
  summarise(
    mean_log_predator_mass = mean(log_predator_mass, na.rm=TRUE),
    median_log_predator_mass = median(log_predator_mass, na.rm=TRUE),
    mean_log_prey_mass = mean(log_prey_mass, na.rm=TRUE),
    median_log_prey_mass = median(log_prey_mass, na.rm=TRUE),
    mean_log_size_ratio = mean(log_size_ratio, na.rm=TRUE),
    median_log_size_ratio = median(log_size_ratio, na.rm=TRUE)
  ) %>%
  rename(Feeding_type = Type.of.feeding.interaction)
# save to csv
write.csv(results, "../results/PP_Results.csv", row.names=FALSE)