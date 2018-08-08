# course_week: 10
# description: given a document term matrix, plot either 
## a) a histogram of the top n most common words, or 
## b) an analysis showing the cumulative frequency of terms
# make generic: maybe

# parameters:
## dtm - a document term matrix created with the tm package
## mode - string. either "top" for graphing the top n values, or "min" to graph values with a minimum frequency of n
build.histogram <- function(dtm, mode = "top", n = 20) {
    library(ggplot2)
    freq <- colSums(as.matrix(dtm)) # make a word-frequency list
    ord <- order(freq, decreasing = TRUE)
    ordered <- freq[ord]
    if (mode == "top") {
        # top mode. plot the top n terms.
        points <- ordered[1:n]
    }
    if (mode == "min") {
        # min mode. plot all terms with a frequency greater than n.
        points <- ordered[ordered > n]
    }
    df <- data.frame(gram = names(points), count = points)
    p <- ggplot(df, aes(gram, count))
    p <- p + geom_bar(stat = "identity")
    p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    p
}

# parameters:
## dtm - a document term matrix created with the tm package
build.pareto <- function(dtm) {
    library(ggplot2)
    freq <- colSums(as.matrix(dtm)) # make word-frequency list
    ord <- order(freq, decreasing = TRUE)
    ordered <- freq[ord]
    # for each entry in the ordered list, convert the entry into a cumulative sum of all entries before it.
    running.total <- sapply(seq_along(ordered), function(x){sum(ordered[1:x])})
    
    # plot the percentage the total for each point in the running total. 
    # the plot will show the x% of total terms are accounted for by the n most frequent words.
    p <- qplot(seq_along(running.total), running.total/running.total[length(running.total)], xlab = "Top Grams", ylab = "Instances (%)")
    p
}
