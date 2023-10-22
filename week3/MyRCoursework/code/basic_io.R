# A simple script to illustrate R input-output.  
# Run line by line and check inputs outputs to understand what is happening  

MyData <- read.csv("/Users/guohongyuan/Desktop/CMEECourseWork/week3/MyRCourseWork/data/trees.csv", header = TRUE) # import with headers

write.csv(MyData, "/Users/guohongyuan/Desktop/CMEECourseWork/week3/MyRCourseWork/data/trees.csv") #write it out as a new file

write.table(MyData[1,], file = "/Users/guohongyuan/Desktop/CMEECourseWork/week3/MyRCourseWork/data/trees.csv",append=TRUE) # Append to it

write.csv(MyData, "/Users/guohongyuan/Desktop/CMEECourseWork/week3/MyRCourseWork/data/trees.csv", row.names=TRUE) # write row names

write.table(MyData, "/Users/guohongyuan/Desktop/CMEECourseWork/week3/MyRCourseWork/data/trees.csv", col.names=FALSE) # ignore column names