pollutantmean <- function(directory, pollutant, id = 1:332, debug = 0) {
  full_df = data.frame()
  for (i in id){
    if (debug == 1) {print(i)}
    padded_id <- formatC(i, width = 3, flag = 0)
    path <- paste(directory, "/", padded_id, ".csv", sep = "")
    if (debug == 1) {print(path)}
    df <- read.csv(path)
    remove_na <- complete.cases(df[[pollutant]])
    cleaned_df <- df[remove_na,]
    if (debug == 1) {print(mean(cleaned_df[, pollutant]))}
    full_df <- rbind(full_df, cleaned_df)
  }
  print(mean(full_df[, pollutant]))
}