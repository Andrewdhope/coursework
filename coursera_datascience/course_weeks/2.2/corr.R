# course_week: 
# description: Loop through a set of files, find the correlation between two variables, given the file holds more observations than a given threshold
# make generic: no
corr <- function(directory, threshold = 0) {
  files <- list.files(directory)
  correlation <- numeric()
  for (i in seq_along(files)) {
    id <- files[i]
    path <- paste(directory, "/", id, sep = "")
    df <- read.csv(path)
    remove_na <- complete.cases(df)
    cleaned_df <- df[remove_na,]
    if (nrow(cleaned_df) >= threshold) {
      correlation <- c(correlation, cor(cleaned_df$sulfate, cleaned_df$nitrate))
    }
  }
  correlation
}