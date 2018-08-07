# course_week: 10
# description: given a document term matrix, plot either 
## a) a histogram of the top n most common words, or 
## b) an analysis showing the cumulative frequency of terms
# make generic: maybe

build.histogram <- function(dtm, mode = "top", n = 20) {
    library(ggplot2)
    freq <- colSums(as.matrix(dtm)) # make a word-frequency list
    ord <- order(freq, decreasing = TRUE)
    ordered <- freq[ord]
    if (mode == "top") {
        points <- ordered[1:n]
    }
    if (mode == "min") {
        points <- ordered[ordered > n]    
    }
    df <- data.frame(gram = names(points), count = points)
    p <- ggplot(df, aes(gram, count))
    p <- p + geom_bar(stat = "identity")
    p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    p
}

build.pareto <- function(dtm) {
    library(ggplot2)
    freq <- colSums(as.matrix(dtm))
    ord <- order(freq, decreasing = TRUE)
    ordered <- freq[ord]
    running.total <- sapply(seq_along(ordered), function(x){sum(ordered[1:x])})
    p <- qplot(seq_along(running.total), running.total/running.total[length(running.total)], xlab = "Top Grams", ylab = "Instances (%)")
    p
}
