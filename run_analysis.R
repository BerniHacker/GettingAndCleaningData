# "Human Activity Recognition Using Smartphones Dataset" 
#
# This script uses the data from the experiments referred in [1]
# 
# [1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and 
# Jorge L. Reyes-Ortiz.
# Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly
# Support Vector Machine. International Workshop of Ambient Assisted Living 
# (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012
# 
# The experiments have been carried out with a group of 30 volunteers. 
# Each person performed six activities (WALKING, WALKING_UPSTAIRS, 
# WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone 
# (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and 
# gyroscope, 3-axial linear acceleration and 3-axial angular velocity have been 
# captured. The obtained dataset has been randomly partitioned into two sets, 
# where 70% of the volunteers was selected for generating the training data and 
# 30% for the test data.
# 
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
# 
run_analysis <- function() {
# Download the files
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", "UCI_HAR_Dataset.zip")
unzip("UCI_HAR_Dataset.zip")
#
# Fetching the data
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
features <- read.table("UCI HAR Dataset/features.txt", stringsAsFactors = FALSE) 
    # The measurement names are stored as character rather than as factor
# Train data
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
X_train <- read.table("UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
# Test data
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
#
# "subject_test" and "subject_train" dataframes contain observations with the 
# IDs of the 30 volunteers. subject_test has 2947 rows and 1 column. 
# subject_train has 7352 rows and 1 column.
#
# "y_test" and "y_train" files dataframes contain the activity of each 
# observation coded as numbers between 1 and 6. The meaning of these numbers is 
# in the "activity_labels" dataframe. y_test has 2947 rows and 1 column. 
# y_train has 7352 rows and 1 column.
#
# "X_test" and "X_train" are dataframes in which rows are observations and 
# every column is a measurement (there are 561 measurements). The names of these
# measurements are in the "features" dataframe. X_test has 2947 rows and 
# X_train has 7352 rows.
# 
# Merging training and test data sets
subject <- rbind(subject_test, subject_train) # 1 column
activity <- rbind(y_test, y_train) # 1 column
measurements <- rbind(X_test, X_train) # 561 columns
all_data <- cbind(subject, activity, measurements) # 563 columns
# Those four new data frame have 10299 rows
#
# Finding out the the measurements on the mean and standard deviation
mean_std <- grep("mean|std", features$V2)
# The integer vector "mean_std" contains 79 elements which provides the rows 
# of the "features" dataframe which corresponds to measurements related to 
# mean or standard deviation
# 
# Extracting the measurements on the mean and standard deviation from the merged
# dataframe "all_data" and storing the result in the new dataframe 
# "mean_std_measurements"
mean_std_measurements <- all_data[,c(1,2,2+mean_std)]
# "mean_std_measurements" has 10299 rows and 81 columns
# 
# Using descriptive  names to name the activities in the "mean_std_measurements"
# data set
mean_std_measurements[,2] <- factor(mean_std_measurements[,2], 
    levels = activity_labels[,1], labels = activity_labels[,2])
#
# Extracting the measurement names corresponding to mean or standard deviation 
# measurements
measurement_names <- features[mean_std,]
# Cleaning the measurement names
measurement_names$V2 <- gsub("[()]", "", measurement_names$V2)
measurement_names$V2 <- gsub("-", ".", measurement_names$V2)
measurement_names$V2 <- gsub("BodyBody", "Body", measurement_names$V2)
measurement_names$V2 <- gsub("Body", "Body.", measurement_names$V2)
measurement_names$V2 <- gsub("Gravity", "Gravity.", measurement_names$V2)
measurement_names$V2 <- gsub("Jerk", ".Jerk", measurement_names$V2)
measurement_names$V2 <- gsub("Mag", ".Magn", measurement_names$V2)
measurement_names$V2 <- gsub("Freq", ".Freq", measurement_names$V2)
measurement_names$V2 <- gsub("Acc", "Accel", measurement_names$V2)
measurement_names$V2 <- gsub("mean", "Mean", measurement_names$V2)
measurement_names$V2 <- gsub("std", "StD", measurement_names$V2)
# Labelling the "mean_std_measurements" data set with descriptive variable names
colnames(mean_std_measurements) <- c("Volunteer.ID", "Activity", 
    measurement_names$V2)
#
# Creating a new tidy data set with the average of each measurement for each 
# subject and each activity
averages <- aggregate(.~Volunteer.ID + Activity, mean_std_measurements, mean)
# "averages" has 180 rows and 81 columns
#
# Exporting the tidy data set
write.table(averages, "averages.txt", row.name=FALSE, quote = FALSE)
                            }
#
# Bernardo Di Chiara, February 17 2018
# R version 3.4.3 on Windows 10