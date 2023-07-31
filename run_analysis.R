setwd("D:/R Files/Coursera/Getting and Cleaning Data/Final Project")
library(dplyr)

# Download data
filename <- "FinalProject.zip"
if(!file.exists(filename)){
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL, filename)
}
if(!file.exists("UCI HAR Dataset")){
  unzip(filename)
}

# Read test and training data and assign all datasets
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))

x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")

x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")

# STEP 1: Merges the training and the test sets to create one data set.
x <- rbind(x_train, x_test)
y <- rbind(y_train, y_test)
subject <-  rbind(subject_train, subject_test)
Merge_df <- cbind(subject, y, x)

# STEP 2: Extracts only the measurements on the mean and standard deviation for each measurement. 
Tidy_df <- Merge_df %>% select(subject, code, contains(c("mean", "std")))

# STEP 3: Uses descriptive activity names to name the activities in the data set
Tidy_df$code <- activities[Tidy_df$code, 2]

# STEP 4: Appropriately labels the data set with descriptive variable names. 
names(Tidy_df)[2] = "activity"
names(Tidy_df)<-gsub("Acc", "Accelerometer", names(Tidy_df))
names(Tidy_df)<-gsub("Gyro", "Gyroscope", names(Tidy_df))
names(Tidy_df)<-gsub("BodyBody", "Body", names(Tidy_df))
names(Tidy_df)<-gsub("Mag", "Magnitude", names(Tidy_df))
names(Tidy_df)<-gsub("^t", "Time", names(Tidy_df))
names(Tidy_df)<-gsub("^f", "Frequency", names(Tidy_df))
names(Tidy_df)<-gsub("tBody", "TimeBody", names(Tidy_df))
names(Tidy_df)<-gsub("-mean()", "Mean", names(Tidy_df))
names(Tidy_df)<-gsub("-std()", "STD", names(Tidy_df))
names(Tidy_df)<-gsub("-freq()", "Frequency", names(Tidy_df))
names(Tidy_df)<-gsub("angle", "Angle", names(Tidy_df))
names(Tidy_df)<-gsub("gravity", "Gravity", names(Tidy_df))

# STEP 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
Tidy_df2 <- Tidy_df %>% group_by(activity, subject) %>% summarize_all(mean)