#################################################
#  Company    : Stevens 
#  Project    : Assignment 2 
#  Purpose    : Solution for Assignment 2
#  First Name : Sayan
#  Last Name  : Mukherjee
#  Id			    : 10430998
#  Date       : 03/05/2018
#  Comments   :

rm(list=ls())
#################################################

# 1. Summarizing each column (e.g. min, max, mean )
BCWisconsin <- read.csv("breast-cancer-wisconsin.data.csv")
summary(BCWisconsin)

# 2. Identifying missing values
BCWisconsin[,1:11][BCWisconsin[,1:11]=="?"] <- NA
is.na(BCWisconsin$Sample)
is.na(BCWisconsin$F1)
is.na(BCWisconsin$F2)
is.na(BCWisconsin$F3)
is.na(BCWisconsin$F4)
is.na(BCWisconsin$F5)
is.na(BCWisconsin$F6)
is.na(BCWisconsin$F7)
is.na(BCWisconsin$F8)
is.na(BCWisconsin$F9)
is.na(BCWisconsin$Class)

# 3. Replacing the missing values with the "mode" (most frequent value) of the column
install.packages("modeest")
library(modeest)

sample_mfv <- mlv(BCWisconsin$Sample, method="mfv", na.rm = TRUE)
BCWisconsin[is.na(BCWisconsin$Sample), "Sample"] <- sample_mfv$M

f1_mfv <- mlv(BCWisconsin$F1, method="mfv", na.rm = TRUE)
BCWisconsin[is.na(BCWisconsin$F1), "F1"] <- f1_mfv$M

f2_mfv <- mlv(BCWisconsin$F2, method="mfv", na.rm = TRUE)
BCWisconsin[is.na(BCWisconsin$F2), "F2"] <- f2_mfv$M

f3_mfv <- mlv(BCWisconsin$F3, method="mfv", na.rm = TRUE)
BCWisconsin[is.na(BCWisconsin$F3), "F3"] <- f3_mfv$M

f4_mfv <- mlv(BCWisconsin$F4, method="mfv", na.rm = TRUE)
BCWisconsin[is.na(BCWisconsin$F4), "F4"] <- f4_mfv$M

f5_mfv <- mlv(BCWisconsin$F5, method="mfv", na.rm = TRUE)
BCWisconsin[is.na(BCWisconsin$F5), "F5"] <- f5_mfv$M

f6_mfv <- mlv(BCWisconsin$F6, method="mfv", na.rm = TRUE)
BCWisconsin[is.na(BCWisconsin$F6), "F6"] <- f6_mfv$M

f7_mfv <- mlv(BCWisconsin$F7, method="mfv", na.rm = TRUE)
BCWisconsin[is.na(BCWisconsin$F7), "F7"] <- f7_mfv$M

f8_mfv <- mlv(BCWisconsin$F8, method="mfv", na.rm = TRUE)
BCWisconsin[is.na(BCWisconsin$F8), "F8"] <- f8_mfv$M

f9_mfv <- mlv(BCWisconsin$F9, method="mfv", na.rm = TRUE)
BCWisconsin[is.na(BCWisconsin$F9), "F9"] <- f9_mfv$M

class_mfv <- mlv(BCWisconsin$Class, method="mfv", na.rm = TRUE)
BCWisconsin[is.na(BCWisconsin$Class), "Class"] <- class_mfv$M

# 4. Displaying the frequency table of "Class" vs. F6
freq_table <- table(unlist(BCWisconsin["Class"]), unlist(BCWisconsin["F6"]))
ftable(freq_table)

# 5. Displaying the scatter plot of F1 to F6, one pair at a time
plot(BCWisconsin[,2], BCWisconsin[,3], main = "F1 vs F2", pch = 20)
plot(BCWisconsin[,2], BCWisconsin[,4], main = "F1 vs F3", pch = 20)
plot(BCWisconsin[,2], BCWisconsin[,5], main = "F1 vs F4", pch = 20)
plot(BCWisconsin[,2], BCWisconsin[,6], main = "F1 vs F5", pch = 20)
plot(BCWisconsin[,2], BCWisconsin[,7], main = "F1 vs F6", pch = 20)

# 6. Show histogram box plot for columns F7 to F9
boxplot(BCWisconsin[8:10])

# Delete all the objects from your R- environment. 
# Reload the "breast-cancer-wisconsin.data.csv" from canvas into R. 
# Remove any row with a missing value in any of the columns.
rm(list=ls())
BCWisconsin <- read.csv("breast-cancer-wisconsin.data.csv")
BCWisconsin[,1:11][BCWisconsin[,1:11]=="?"] <- NA
BCWisconsin <- na.omit(BCWisconsin)
