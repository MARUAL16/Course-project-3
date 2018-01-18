################################################
#Title: Course Project Getting and cleanig Data
################################################

#0.Description of the project:

#The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:



#0.1.GET DATA:

#0.1.1. Download zip file containing data if it hasn't already been downloaded:

zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "CourseProject3.zip"

if (!file.exists(zipFile)) {
download.file(zipUrl, zipFile, mode = "wb")
}

#0.1.2 Unzip zip file containing data if it hasn't already been downloaded:

dataPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
  unzip(zipFile)
} 


#0.2.READ DATA:

#0.2.1 Read training data

subjectTrain <- read.table(file.path(dataPath, "train", "subject_train.txt"))
xtrain <- read.table(file.path(dataPath, "train", "X_train.txt"))
ytrain <- read.table(file.path(dataPath, "train", "y_train.txt"))
train <- cbind(subjectTrain,xtrain,ytrain)

#0.2.2 Read test data

subjectTest <- read.table(file.path(dataPath, "test", "subject_test.txt"))
xtest <- read.table(file.path(dataPath, "test", "X_test.txt"))
ytest<- read.table(file.path(dataPath, "test", "y_test.txt"))
test <- cbind(subjectTest,xtest,ytest)

#0.2.3.  Read features

features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)


#0.2.4. Read activity labels

activities <- read.table(file.path(dataPath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")



#1. Merge both data tables (train and test) to make a single data table

humanActivity <- rbind(train,test)


#1.1 Assignment the column names 

colnames(humanActivity) <- c("subject", features[, 2], "activity")



#2. Extract only the measurements on the mean and standard deviation for each measurement


#2.1 Determination of the  columns of the data set to keep. This step it's based on column names
columnsToKeep <- grepl("subject|activity|mean|std", colnames(humanActivity))

#2.2 Keep only the data of the column determined in the previous step

humanActivity <- humanActivity[, columnsToKeep]


#3 Use descriptive activity names to name the activities in the data set


#3.1. Replace activity values (levels) with their respective labels 
humanActivity$activity <- factor(humanActivity$activity,levels = activities[, 1], labels = activities[, 2])

#4.Appropriately label the data set with descriptive variable names


#4.1 Get column names
humanActivityCols <- colnames(humanActivity)

#4.2 Remove special characters
humanActivityCols <- gsub("[\\(\\)-]", "", humanActivityCols)

#4.3 Expand abbreviations and clean up names
humanActivityCols <- gsub("^f", "frequencyDomain", humanActivityCols)
humanActivityCols <- gsub("^t", "timeDomain", humanActivityCols)
humanActivityCols <- gsub("Acc", "Accelerometer", humanActivityCols)
humanActivityCols <- gsub("Gyro", "Gyroscope", humanActivityCols)
humanActivityCols <- gsub("Mag", "Magnitude", humanActivityCols)
humanActivityCols <- gsub("Freq", "Frequency", humanActivityCols)


#4.4 Use new labels as column names
colnames(humanActivity) <- humanActivityCols



#5. Create a second, independent tidy data set with the average of each variable for each activity and each subject

Data2<-aggregate(.~ subject + activity,humanActivity, mean)
Data2<-Data2[order(Data2$subject,Data2$activity),]
write.table(Data2, file = "tidydata.txt",row.name=FALSE)


