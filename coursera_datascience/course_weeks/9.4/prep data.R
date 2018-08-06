prepdata <- function() {
    
library(dplyr)
library(purrr)
library(nbastatR)

df.sal <- read.csv("NBASalaryRaw.csv")
# traded players will repeat. will need to combine salaires when we get there (maybe at this point).
df.sal <- df.sal[,2:4]
df.sal$X2017.18 <- as.character(df.sal$X2017.18)
df.sal$X2017.18 <- gsub("\\$", "", df.sal$X2017.18)
df.sal$X2017.18 <- gsub("\\,", "", df.sal$X2017.18)
df.sal$X2017.18 <- as.numeric(df.sal$X2017.18)
aggregate(X2017.18 ~ Player, data = df.sal, FUN = sum)
players <- df.sal$Player
players <- as.character(players)
p <- head(players, 151) #150
df.stat <- get_players_career_stats(players = p, modes = c("Totals"), assign_to_environment = FALSE, return_message = TRUE) 
df.sub <- subset(df.stat, nameTable == "CareerTotalsRegularSeason")[,c(3,5)]
# convert the tibble to a data frame with map, lapply, as.data.frame, and cbind.
df.tib <- lapply(df.sub$dataTable, as.data.frame)
df.use <- data.frame()
df.tib

for (i in c(1:length(df.tib))){
     if (length(df.tib[[i]] == 24)) { df.tib[[i]]$pctFG3 <- 0}
     df.use <- rbind(df.use, df.tib[[i]])
 }
df.use <- cbind(df.sub$namePlayer, df.use)
names(df.use)[1] <- "Player"
df.join <- inner_join(df.sal, df.use, by = c("Player"))
names(df.join)[3] <- "Salary"
df.join
}