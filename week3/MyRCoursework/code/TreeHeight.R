
# This function calculates heights of trees given distance of each tree 
# from its base and angle to its top, using  the trigonometric formula 
#
# height = distance * tan(radians)
#
# ARGUMENTS
# degrees:   The angle of elevation of tree
# distance:  The distance from base of tree (e.g., meters)
#
# OUTPUT
# The heights of the tree, same units as "distance"

TreeHeight <- function(degrees, distance) {
  radians <- degrees * pi / 180
  height <- distance * tan(radians)
  print(paste("Tree height is:", height))
  
  return (height)
}

# Load data from trees.csv in the "data" folder
trees_data <- read.csv("../data/trees.csv")

# Calculate tree heights
trees_data$Tree.Height.m <- mapply(TreeHeight, trees_data$Angle.degrees, trees_data$Distance.m)

# Create a new data frame with the desired output format
output_data <- trees_data %>% 
  select(Species, Distance.m, Angle.degrees, Tree.Height.m)

# Write the output TreeHts.csv in the "results" folder
write.csv(output_data, "../results/TreeHts.csv", row.names = FALSE)