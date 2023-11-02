library(ggplot2)
library(dplyr)

# Read the data
MyDF <- read.csv("../data/EcolArchives-E089-51-D1.csv")
# Obtain regression results
get_regression_results <- function(data_subset) {
  if(nrow(data_subset) > 1) {
    model <- lm(Predator.mass ~ Prey.mass, data = data_subset)
    coefficients <- coef(summary(model))
    
    if("Prey.mass" %in% rownames(coefficients)) {
      slope <- coefficients["Prey.mass", "Estimate"]
      intercept <- coefficients["(Intercept)", "Estimate"]
      p_value <- coefficients["Prey.mass", "Pr(>|t|)"]
      
      r_squared <- summary(model)$r.squared
      f_statistic <- summary(model)$fstatistic[1]
    } else {
      slope <- NA
      intercept <- NA
      p_value <- NA
      r_squared <- NA
      f_statistic <- NA
    }
  } else {
    slope <- NA
    intercept <- NA
    p_value <- NA
    r_squared <- NA
    f_statistic <- NA
  }
  
  data.frame(
    slope = slope,
    intercept = intercept,
    r_squared = r_squared,
    f_statistic = f_statistic,
    p_value = p_value
  )
}


# Perform linear regression analysis on each subset
results <- MyDF %>%
  group_by(Type.of.feeding.interaction, Predator.lifestage) %>%
  do(get_regression_results(.))

# 
write.csv(results, "../results/PP_Regress_Results.csv", row.names = FALSE)

# Visualizing the regression analyses
pdf("../results/regression_plot.pdf")

ggplot(MyDF, aes(x = Prey.mass, y = Predator.mass)) +
  facet_grid(Type.of.feeding.interaction ~ .) + 
  geom_point(aes(color = Predator.lifestage), alpha = 0.5) +
  geom_smooth(method = "lm", aes(color = Predator.lifestage), se = FALSE, fullrange = TRUE) +
  scale_y_log10(breaks = c(1e-6, 1e-2, 1e+2, 1e+6),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_x_log10(breaks = c(1e-07, 1e-03, 1e+01),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  labs(title = "Regression analyses subsetted by Predator lifestage",
       x = "Prey Mass in grams",
       y = "Predator Mass in grams") +
  theme_minimal() +
  theme(legend.position="bottom",
        panel.border = element_rect(fill = NA, color = "black", linewidth = 1)) +  # Added this line
  guides(color = guide_legend(title = "Predator.lifestage", ncol = 6, keywidth = 1, keyheight = 1))

dev.off()
