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
#
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


#
# gram: string to solve for. currently assuming 3-word string.
# mode: 1-blogs, 2-twitter, 3-news, 4-all
#
solve <- function(gram, mode, n) {
    filename.dtm <- makefilename.dtm(mode, n)
    filename.freq <- makefilename.freq(mode)
    load.dtm <- load(filename.dtm)
    load.freq <- load(filename.freq)
    candidate.list <- next.gram.candidates(dtm, gram)
    probs <- candidate.to.dist(candidate.list, top.grams)
    probs <- sort(probs, decreasing = TRUE)
    probs
}

# version 2 - load dtms from file, with "back-off" methods. return probability compared to a freq_list.
solve2 <- function(gram, mode) {
    path <- 'C:/Users/ahope/Desktop/_MyFiles/repos/coursework/coursera_datascience/course_weeks/10/files'
    setwd(path)
    filename.freq <- makefilename.freq(mode)
    load.freq <- load(filename.freq)
    len <- length(strsplit(gram, " ")[[1]])
    
    for (n in c(3:1)) {
        n.gram <- character()    
        filename.dtm <- makefilename.dtm(mode, n)
        load.dtm <- load(filename.dtm)
        if (n == 1) {
            n.gram <- strsplit(gram, " ")[[1]][len]
        }
        else {
            for (i in c((len-n+1):len)) {
                n.gram <- paste(n.gram, strsplit(gram, " ")[[1]][i], sep = " ")
            }
        }
        n.gram <- trimws(n.gram) # trim leading space
        print(n.gram)
        print(filename.dtm)
        candidate.list <- next.gram.candidates(dtm, n.gram)
        if (length(candidate.list) > 0) {
            break} # UGLY. WILL REPLACE WITH TRY-CATCH.
    }
    probs <- candidate.to.dist(candidate.list, top.grams)
    probs <- sort(probs, decreasing = TRUE)
    probs
}

# version 3 - use next.gram.candidates2, return the candidate list rather than the probability list. 
solve3 <- function(gram, mode) {
    path <- 'C:/Users/ahope/Desktop/_MyFiles/repos/coursework/coursera_datascience/course_weeks/10/files'
    setwd(path)
    filename.freq <- makefilename.freq(mode)
    load.freq <- load(filename.freq)
    len <- length(strsplit(gram, " ")[[1]])
    
    for (n in c(3:1)) {
        n.gram <- character()    
        filename.dtm <- makefilename.dtm(mode, n)
        load.dtm <- load(filename.dtm)
        if (n == 1) {
            n.gram <- strsplit(gram, " ")[[1]][len]
        }
        else {
            for (i in c((len-n+1):len)) {
                n.gram <- paste(n.gram, strsplit(gram, " ")[[1]][i], sep = " ")
            }
        }
        n.gram <- trimws(n.gram) # trim leading space
        print(n.gram)
        print(filename.dtm)
        candidate.list <- next.gram.candidates2(dtm, n.gram)
        # if ((length(candidate.list[[1]]>0))&&(candidate.list[[1]][1] > 0)) {
        if (length(candidate.list) > 0) {
            break} # UGLY. WILL REPLACE WITH TRY-CATCH.
    }
    candidate.list
}


makefilename.dtm <- function(mode, n) {
    #if (mode==1) {modefile <- "abbrev.blogs.txt"}
    if (mode==1) {modefile <- "60K.blogs.txt"}
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

makefilename.freq <- function(mode) {
    if (mode==1) {filename <- "freq_blogs.txt.gz"}
    if (mode==2) {filename <- "freq_twitter.txt.gz"}
    if (mode==3) {filename <- "freq_news.txt.gz"}
    if (mode==4) {}
    filename
}

write.new.set <- function(readname, writename, limit) {
    reader.setwd()
    write.abbrev(readname, writename, limit)
    for (i in c(3:1)) {
    compress(writename, i)
    }
}
