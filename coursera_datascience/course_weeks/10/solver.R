 
#
# given a nxn.dtm and a string, return possible next-grams
# start with by 4x3 and go from there
# 
next.gram.candidates <- function(dtm, gram) {
    candidate.list <- list()
    result = tryCatch({
        pairs <- dimnames(dtm)$Terms[dtm[dtm[, gram]$i,]$j]
    },  error = function(e) {
        return(candidate.list)
    })
    if (is.list(result)) {return(candidate.list)}
    #pairs <- dimnames(dtm)$Terms[dtm[dtm[, gram]$i,]$j]
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
                # check if it is already on the candidate list. 
                # <<< design decision >>>
                #
                if (! listsplit[[1]][listlen] %in% candidate.list) {
                candidate.list <- append(candidate.list, listsplit[[1]][listlen]) 
                }
            }
        }
    }
    candidate.list
}


next.gram.candidates2 <- function(dtm, gram) {
    candidate.list <- list()
    #browser()
    result = tryCatch({
        pairs <- dimnames(dtm)$Terms[dtm[dtm[, gram]$i,]$j]
    },  error = function(e) {
        return(candidate.list)
    })
    if (is.list(result)) {return(candidate.list)}
    #candidate.list <- list(1)
    candidate.list[[1]] <- list()
    #pairs <- dimnames(dtm)$Terms[dtm[dtm[, gram]$i,]$j]
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
    # candidate.list
    names(candidate.list[[length(candidate.list)]])
}


#
# given a list of candidate unigrams, return a named list that pairs the value with its relative frequency as expressed as a probability
#
candidate.to.dist <- function(candidate.list, top.grams) {
    probs <- numeric()
    titles <- character()
    total <- 0
    for (i in seq_along(candidate.list)){
        if (!is.null(top.grams[candidate.list[[i]]][[1]])) {
            probs <- append(probs, top.grams[candidate.list[[i]]][[1]])
            titles <- append(titles, candidate.list[[i]])
            total <- total + top.grams[candidate.list[[i]]][[1]]
        }
    }
    names(probs) <- titles
    probs <- probs/total
    probs
}
