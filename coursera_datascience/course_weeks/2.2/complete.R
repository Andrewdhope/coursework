# course_week:
# description: Loop through a set of files, count the number of complete observations and store them to a data frame.
# make generic: no
complete <- function(directory, id = 1:332) {
    count_obs = data.frame()
    for (i in id){
      padded_id <- formatC(i, width = 3, flag = 0)
      path <- paste(directory, "/", padded_id, ".csv", sep = "")
      df <- read.csv(path)
      remove_na <- complete.cases(df)
      cleaned_df <- df[remove_na,]
      count_obs <- rbind(count_obs, c(i, nrow(cleaned_df)))
    }
    colnames(count_obs) <- c("id", "nobs")
    count_obs
}