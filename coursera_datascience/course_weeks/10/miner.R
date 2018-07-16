
## PREPROCESSING ##

clean.corpus <- function(corpus) {
    corpus <- tm_map(corpus, removePunctuation)
    repair.apostrophe <- content_transformer(function(x) {return (gsub("â€™", "'", x))})
    #remove.hashtags <- content_transformer(function(x) {return (gsub("#", "", x))})
    #remove.at <- content_transformer(function(x) {return})
    corpus <- tm_map(corpus, repair.apostrophe)
    #tm_map(corpus, remove.hashtags)
    #tm_map(corpus, remove.at)
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus
}


#
# freq.list
#   return a list of unique words that account for a % of all words in a dtm
#   CHECK IF THIS STRIPS OUT TWO-LETTER WORDS
freq.list <- function(dtm, threshold = 0.85) {
    freq <- colSums(as.matrix(dtm))
    ord <- order(freq, decreasing = TRUE)
    ordered <- freq[ord]
    
    #initialize variables
    i <- 1
    done <- FALSE
    running.total <- 0
    
    while (done != TRUE) {
        total.grams <- sum(freq)
        running.total <- running.total + ordered[[i]]
        if ((running.total / total.grams) > threshold) {done = TRUE}
        i <- i+1
    }
    top.grams <- as.list(ordered[1:i])
    top.grams
}


#
# given a cleaned corpus, make a corpus with the new documents being tokenized n-grams of the incoming documents.
#
make.ngram.corpus <- function(corpus, n) {
    object <- sapply(corpus, strwrap) # convert corpus to vector
    object <- sapply(object, Boost_tokenizer) # tokenize
    object <- unlist(object, use.names = T)
    object <- ngrams(object, n)
    object <- VCorpus(VectorSource(object))
    object
}

#
# given a corpus and a tokenizer function, make a DTM with documents of n-grams x tokenized (n-1)-grams
# corpus should be a set of n-gram documents (from make.ngram.corpus)
# taken from http://tm.r-forge.r-project.org/faq.html#Bigrams
#
make.nxn.dtm <- function(corpus, tokenizer) {
    DocumentTermMatrix(corpus, control = list(tokenize = tokenizer))
}


unigram.tokenizer <- function(x) {
    unlist(lapply(ngrams(words(x), 1), paste, collapse = " "), use.names = FALSE)   
}

bigram.tokenizer <- function(x) {
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)   
}

trigram.tokenizer <- function(x) {
    unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = TRUE)   
}

quadram.tokenizer <- function(x) {
    unlist(lapply(ngrams(words(x), 4), paste, collapse = " "), use.names = FALSE)   
}

generic.tokenizer <- function(x, n) {
    unlist(lapply(ngrams(words(x), n), paste, collapse = " "), use.names = FALSE)   
}

## USEFUL FUNCTIONS ##

# VCorpus(VectorSource(vector))
# DocumentTermMatrix(corpus)
# TermDocumentMatrix(corpus)

# strwrap(b.corpus[[1]])
# sapply(b.corpus, strwrap)
