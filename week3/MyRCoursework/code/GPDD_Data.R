library(maps)

# Load the GPDD data
load("../data/GPDDFiltered.RData")
ls()
str(gpdd)

# Creating a World Map
map("world", fill=TRUE, col="white", bg="blue")

# Mark the coordinates in red in the dataset
points(gpdd$long, gpdd$lat, pch=19, col="red", cex=1.5, bg="yellow")



