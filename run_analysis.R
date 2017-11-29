##This script is to create run_analys.R to do the course assignment

##Use library
library(data.table)
library(dplyr)

##Merges the training and the test sets to create one data set.
featurename <- read.table("UCI HAR Dataset/features.txt")
activitylabel <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)
subjecttrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activitytrain <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
featurestrain <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)
subjecttest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activitytest <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
featurestest <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)
subject <- rbind(subjecttrain, subjecttest)
activity <- rbind(activitytrain, activitytest)
features <- rbind(featurestrain, featurestest)

##Uses descriptive activity names to name the activities in the data set
colnames(features) <- t(featurename[2])
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)

##Extracts only the measurements on the mean and standard deviation for each measurement.
columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)
requiredColumns <- c(columnsWithMeanSTD, 562, 563)
dim(completeData)
extractedData <- completeData[,requiredColumns]
dim(extractedData)
extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}
extractedData$Activity <- as.factor(extractedData$Activity)

##Appropriately labels the data set with descriptive variable names.
names(extractedData)
names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))
extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)

##creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)
