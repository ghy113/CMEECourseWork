################################################################
################## Wrangling the Pound Hill Dataset ############
################################################################

library(tidyverse)  # Load the tidyverse package

############# Load the dataset ###############
# header = false because the raw data don't have real headers
MyData <- as.matrix(read.csv("../data/PoundHillData.csv", header = FALSE))

# header = true because we do have metadata headers
MyMetaData <- read.csv("../data/PoundHillMetaData.csv", header = TRUE, sep = ";")

############# Inspect the dataset ###############
head(MyData)
dim(MyData)
str(MyData)
fix(MyData) #you can also do this
fix(MyMetaData)

############# Transpose ###############
# To get those species into columns and treatments into rows 
MyData <- t(MyData) 
head(MyData)
dim(MyData)

############# Replace species absences with zeros ###############
MyData[MyData == ""] = 0

############# Convert raw matrix to data frame ###############

TempData <- as.data.frame(MyData[-1,], stringsAsFactors = F) #stringsAsFactors = F is important!
colnames(TempData) <- MyData[1,] # assign column names from original data

############# Convert from wide to long format using tidyr's gather ###############

MyWrangledData <- TempData %>%
  gather(key = "Species", value = "Count", -c(Cultivation, Block, Plot, Quadrat))

############# Convert variables to appropriate data types ###############

MyWrangledData <- MyWrangledData %>%
  mutate(
    Cultivation = as.factor(Cultivation),
    Block = as.factor(Block),
    Plot = as.factor(Plot),
    Quadrat = as.factor(Quadrat),
    Count = as.integer(Count)
  )

############# Inspect the wrangled data ###############
str(MyWrangledData)
head(MyWrangledData)
dim(MyWrangledData)


