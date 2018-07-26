 
next.gram.candidates2 <- function(dtm, gram) {
    candidate.list <- list()
    #browser()
    result = tryCatch({
        pairs <- dimnames(dtm)$Terms[dtm[dtm[, gram]$i,]$j]
    },  error = function(e) {
        return(candidate.list)
    })
    if (is.list(result)) {return(candidate.list)}
    
    # load.freq <- load("freq_blogs.txt.gz")
    # top.grams
    
    #candidate.list <- list(1)
    candidate.list[[1]] <- list()
    pair.list <- strsplit(pairs, "\"") 
    for (i in seq_along(pair.list)) {
        if (pair.list[[i]] != gram) {
            gramsplit <- strsplit(gram, " ")
            listsplit <- strsplit(pair.list[[i]], " ")
            gramlen <- length(gramsplit[[1]])
            listlen <- length(listsplit[[1]])
            # the given gram needs to be the first part of the pair
            if (all(listsplit[[1]][2:listlen] != gramsplit[[1]][1:gramlen-1])) {
                #
                # list will now be indexed by count -- list[1]$words, list[2]$words, etc.
                # loop backward over this list, when gram is found, add it to the next-highest index value.
                # return the highest-order sublist and roll with that
                # this will improve the returns for high-volume matches, and won't impact low-volume ones.
                #
                for (j in c(length(candidate.list):1)) {
                    if (listsplit[[1]][listlen] %in% names(candidate.list[[j]])) {
                        # create next level if it doesn't exist
                        if (j == length(candidate.list)) {
                            candidate.list[[j+1]] <- list()  
                        }
                        candidate.list[[j+1]][[listsplit[[1]][listlen]]] = 1
                        # somewhat inefficient to have high-repeating grams listed at every level
                        # try to remove them...
                        # ...but dont mug yourself...
                        break
                    }
                    if (j == 1) {
                        candidate.list[[j]][[listsplit[[1]][listlen]]] = 1  
                    }
                    
                }
            }
        }
    }
    #print("candidate.list: ", quote = FALSE) # -- these next two lines need a little work. Just want to print the winner and their "score".
    #print(candidate.list)
    #candidate.list
    names(candidate.list[[length(candidate.list)]])
}