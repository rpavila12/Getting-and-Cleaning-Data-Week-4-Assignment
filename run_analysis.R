library(dplyr)
library(xlsx)
library(foreign)
library(haven)
# train data
XTrain <- read.table("./UCI HAR Dataset/train/X_train.txt")
YTrain <- read.table("./UCI HAR Dataset/train/Y_train.txt")
SubTrain <- read.table("./UCI HAR Dataset/train/subject_train.txt")

# test data
XTest <- read.table("./UCI HAR Dataset/test/X_test.txt")
YTest <- read.table("./UCI HAR Dataset/test/Y_test.txt")
SubTest <- read.table("./UCI HAR Dataset/test/subject_test.txt")

# features
Features <- read.table("./UCI HAR Dataset/features.txt")

# activity labels
ActivityLabels <- read.table("./UCI HAR Dataset/activity_labels.txt")

# 1. Merges the training and the test sets to create one data set.
XTotal <- rbind(XTrain, XTest)
YTotal <- rbind(YTrain, YTest)
SubTotal <- rbind(SubTrain, SubTest)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
SelectFeatures <- Features[grep("mean\\(\\)|std\\(\\)",Features[,2]),]
XTotal <- XTotal[,SelectFeatures[,1]]

# 3. Uses descriptive activity names to name the activities in the data set
colnames(YTotal) <- "activity"
YTotal$activitylabel <- factor(YTotal$activity, labels = as.character(ActivityLabels[,2]))
activitylabel <- YTotal[,-1]

# 4. Appropriately labels the data set with descriptive variable names.
colnames(XTotal) <- Features[SelectFeatures[,1],2]

# 5. From the data set in step 4, creates a second, independent tidy data set with the average
# of each variable for each activity and each subject.
colnames(SubTotal) <- "subject"
Total <- cbind(XTotal, activitylabel, SubTotal)
TotalMean <- Total %>% group_by(activitylabel, subject) %>% summarise_all(funs(mean))
write.table(TotalMean, file = "./UCI HAR Dataset/tidydata.txt", row.names = FALSE, col.names = TRUE)
write.xlsx(as.data.frame(TotalMean), file = "./UCI HAR Dataset/tidydata.xlsx", sheetName="Sheet1", col.names=TRUE, row.names=TRUE, append=FALSE)
write.dta(as.data.frame(TotalMean), file = "./UCI HAR Dataset/tidydata.dta")
write.foreign(as.data.frame(TotalMean), "./UCI HAR Dataset/tidydata.txt", "./UCI HAR Dataset/tidydata.sps", package = c("SPSS"))
write_sav(as.data.frame(TotalMean), "./UCI HAR Dataset/tidydata.sav")