Author: Andrew Hope

Coursera - Data Science - Johns Hopkins University  
Course 3 - Getting and Cleaning Data - Week 4 Project

=========

The project contains a single R script containing a single function, run_analysis(). 

The script will access files in the UCI HAR Dataset. It will first combine separate tables into one tidy dataset, 
which contains a subject ID, an activity ID and the set of all measurements taken in the experiment.
Each subject-activity pair has several recordings for each measurement, corresponding to short time intervals during the experiment.

Several variables are then removed, leaving only variables of calculated mean or standard deviation of each measured variable.

After the data is collected and labelled, the script splits the dataset on subject, and on activity. 
The script then builds a new tidy dataset that contains only the mean of each remaining variable for every subject-activity pair.

Labels are then assigned as variable names, and the activity names are inserted in place of the numeric factor.


Dataset source: 
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

Details on the experimental design, the observations collected, and variables used, can be found in the CodeBook.md, 
as well as in the features_info.txt and README files of the original data set.
