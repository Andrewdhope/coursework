# course_week: 2.2
# description: Loop through a set of files, find the mean of one of the file's columns, and put the mean of a file as a row in a data frame
# make generic: no
pollutantmean <- function(directory, pollutant, id = 1:332, debug = 0) {
  full_df = data.frame()
  for (i in id){
    if (debug == 1) {print(i)}
    padded_id <- formatC(i, width = 3, flag = 0) # pad a number with leading zeros
    path <- paste(directory, "/", padded_id, ".csv", sep = "") # paste to concatenate a file path
    if (debug == 1) {print(path)}
    df <- read.csv(path) # read csv into a data frame
    remove_na <- complete.cases(df[[pollutant]]) # complete cases only
    cleaned_df <- df[remove_na,]
    if (debug == 1) {print(mean(cleaned_df[, pollutant]))}
    full_df <- rbind(full_df, cleaned_df)
  }
  print(mean(full_df[, pollutant]))
}