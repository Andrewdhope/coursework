# You should create one R script called run_analysis.R that does the following.
# 
# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement.
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names.
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
run_analysis <- function() {
  
  # read test data from their directories in the dataset
  x_test <- read.table("test/X_test.txt", sep = "")
  y_test <- read.table("test/y_test.txt", sep = "")
  
  # combine the y-column to the front of the x-dataset
  xy_test <- cbind(y_test, x_test) 
  
  # repeat with training data
  x_train <- read.table("train/X_train.txt", sep = "")
  y_train <- read.table("train/y_train.txt", sep = "")
  xy_train <- cbind(y_train, x_train)
  
  # combine the test and train datasets
  xy_full <- rbind(xy_train, xy_test)
}