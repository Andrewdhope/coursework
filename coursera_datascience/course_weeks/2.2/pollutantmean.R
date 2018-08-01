# course_week: 2.2 
# description: Loop through a set of files, combine all complete cases to a new data frame
# make generic: yes
pollutantmean <- function(directory, pollutant, id = 1:332) {
  full_df = data.frame()
  for (i in id){
    padded_id <- formatC(i, width = 3, flag = 0)
    path <- paste(directory, "/", padded_id, ".csv", sep = "")
    df <- read.csv(path)
    remove_na <- complete.cases(df[[pollutant]])
    cleaned_df <- df[remove_na,]
    full_df <- rbind(full_df, cleaned_df)
  }
  mean(full_df[, pollutant])
}