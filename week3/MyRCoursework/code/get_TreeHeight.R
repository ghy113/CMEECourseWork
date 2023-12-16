# Define the function to calculate tree height
TreeHeight <- function(degrees, distance) {
    radians <- degrees * pi / 180
    height <- distance * tan(radians)
    return (height)
}

# Obtain the command line arguments
args <- commandArgs(trailingOnly = TRUE)

# Ensure a file is provided as an argument
if(length(args) == 0) {
    stop("No file provided. Usage: get_TreeHeight.R <filename.csv>")
}

input_file <- args[1]

# Load the data
data <- read.csv(input_file, header=TRUE)

# Calculate tree heights
data$Tree.Height.m <- mapply(TreeHeight, data$Angle.degrees, data$Distance.m)

# Extract the base file name without extension and path
base_name <- tools::file_path_sans_ext(basename(input_file))

# Create the output file name
output_file <- paste0("../results/", base_name, "_treeheights.csv")

# Write the combined data to the output file
write.csv(data, file=output_file, row.names=FALSE)
