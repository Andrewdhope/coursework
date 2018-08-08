# author: andrew hope
# course_week: 10
# description: functions to execute the business logic from start to finish.
## written for the Coursera Data Science Specialization Capstone in July 2018.
# make generic: no

source('C:/Users/ahope/Desktop/_MyFiles/repos/coursework/coursera_datascience/course_weeks/10/reader.R')
source('C:/Users/ahope/Desktop/_MyFiles/repos/coursework/coursera_datascience/course_weeks/10/miner.R')
source('C:/Users/ahope/Desktop/_MyFiles/repos/coursework/coursera_datascience/course_weeks/10/solver.R')
source('C:/Users/ahope/Desktop/_MyFiles/repos/coursework/coursera_datascience/course_weeks/10/plotter.R')
library(tm)

init <- function() {
    reader.setwd()   
}

main <- function(filename) {
    reader.setwd()
    text <- reader.all(filename)
    text
}


#
# create a set of (m x n) document term matrices used for gram lookup.
# lower order (m x n) dtms are used for a back-off procedure.
# n: the n-grams that this compressed file will be used to predict (1, 2, 3)    
#
#
compress <- function(filename, n) {
    library(tm)
    text <- main(filename)
    corpus <- VCorpus(VectorSource(text))
    corpus <- clean.corpus(corpus)
    corpus <- make.ngram.corpus(corpus, n+1)
    if (n==1) {tokenizer <- unigram.tokenizer}
    if (n==2) {tokenizer <- bigram.tokenizer}
    if (n==3) {tokenizer <- trigram.tokenizer}
    if (n==4) {tokenizer <- quadram.tokenizer}
    dtm <- make.nxn.dtm(corpus, tokenizer)
    # dtm <- removeSparseTerms(dtm, 0.999) # new experiment, same poor results
    savename <- paste(n+1, n, sep = "x")
    savename <- paste(savename, "dtm", sep = "-")
    savename <- paste(savename, filename, sep = "_")
    savename <- paste(savename, "gz", sep = ".")
    path <- 'C:/Users/ahope/Desktop/_MyFiles/repos/coursework/coursera_datascience/course_weeks/10/files'
    setwd(path)
    save(dtm, file = savename, compress = TRUE)
}


solve3 <- function(gram, mode) {
    path <- 'C:/Users/ahope/Desktop/_MyFiles/repos/coursework/coursera_datascience/course_weeks/10/files'
    setwd(path)
    len <- length(strsplit(gram, " ")[[1]])
    
    for (n in c(3:1)) {
        n.gram <- character()    
        filename.dtm <- makefilename.dtm(mode, n)
        load.dtm <- load(filename.dtm)
        # if n is one, always take the last word in the gram
        if (n == 1) {
            n.gram <- strsplit(gram, " ")[[1]][len]
        }
        # else take the last n words in the given gram
        else {
            for (i in c((len-n+1):len)) {
                n.gram <- paste(n.gram, strsplit(gram, " ")[[1]][i], sep = " ")
            }
        }
        n.gram <- trimws(n.gram) # trim leading space
        # print(n.gram)
        # print(filename.dtm)
        candidate.list <- next.gram.candidates2(dtm, n.gram) 
        if (length(candidate.list) > 0) {
            break} 
    }
    # use this hueristic if the loop above doesn't produce a candidate
    if (n == 1) {
        load("freq.dist.txt.gz")
        candidate.list[[1]] <- 1
        candidate.list <- sample(names(top.dist), 1, prob = top.dist) # if candiate.list doesn't return values, take a random stopword.
    }
    candidate.list
}


makefilename.dtm <- function(mode, n) {
    #if (mode==1) {modefile <- "abbrev.blogs.txt"}
    if (mode==1) {modefile <- "50K.blogs.txt"}
    #if (mode==2) {modefile <- "abbrev.twitter.txt"}
    if (mode==2) {modefile <- "30K.twitter.txt"}
    #if (mode==3) {modefile <- "abbrev.news.txt"}
    if (mode==3) {modefile <- "20K.news.txt"}
    if (mode==4) {}
    filename <- paste(n+1, n, sep = "x")  
    filename <- paste(filename, "dtm", sep = "-")
    filename <- paste(filename, modefile, sep = "_")
    filename <- paste(filename, "gz", sep = ".")
    filename
}

#
# 
# 

write.new.set <- function(readname, writename, limit) {
    reader.setwd()
    write.abbrev(readname, writename, limit)
    for (i in c(3:1)) {
    compress(writename, i)
    }
}
