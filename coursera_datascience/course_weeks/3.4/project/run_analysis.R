# You should create one R script called run_analysis.R that does the following.
# 
# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement.
# Uses descriptive activity names to name the activities in the data set. 
# Appropriately labels the data set with descriptive variable names.
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
run_analysis <- function() {
  
  # read test data from their directories in the dataset
  x_test <- read.table("test/X_test.txt", sep = "")
  y_test <- read.table("test/y_test.txt", sep = "")
  
  # combine the y-column to the left of the x-dataset
  xy_test <- cbind(y_test, x_test) 
  
  # repeat with training data
  x_train <- read.table("train/X_train.txt", sep = "")
  y_train <- read.table("train/y_train.txt", sep = "")
  xy_train <- cbind(y_train, x_train)
  
  # prep subject data as a column
  subject_test <- read.table("test/subject_test.txt", sep = "")
  subject_train <- read.table("train/subject_train.txt", sep = "")
  subject_full <- rbind(subject_train, subject_test)
  
  # combine the test, train, and subject datasets
  xy_full <- rbind(xy_train, xy_test)
  xy_full <- cbind(subject_full, xy_full)
  
  # add the features as column names
  features <- read.table("features.txt", sep = "")
  featureVector <- as.vector(features[[2]])
  featureVector <- c("subjectID", "activityID", featureVector)
  colnames(xy_full) <- featureVector
  
  # include only the mean and std columns
  meanCols <- grep("mean\\(", names(xy_full)) # use mean( as the pattern to avoid picking up the meanFreq columns
  stdCols <- grep("std", names(xy_full))
  xy_part <- xy_full[,c(1, 2, meanCols, stdCols)]

  output <- data.frame()
  # condense to mean values for each subject-activity pair
  s1 <- split(xy_part, xy_part[[1]]) # split on subject
  for(i in 1:length(s1)) {
    si <- s1[[i]]
    s2 <- split(si, si[[2]]) # split on activity
    for(j in 1:length(s2)) {
      avg <- sapply(s2[[j]][,3:length(s2[[j]])], mean) # averages, excluding the first two (non-measurement) columns
      output <- rbind(output, c(i, j, avg))
    }
  }

  # insert the descriptive activity names
  labels <- read.table("activity_labels.txt", sep = "")
  labels <- labels[[2]]
  vals <- output[[2]]
  insert <- cut(vals, 0:6, labels)
  output[[2]] = insert # replace column 2 with the mapped descriptive names

  # restore the column names
  colnames(output) <- colnames(xy_part)
  
  return(output)
}