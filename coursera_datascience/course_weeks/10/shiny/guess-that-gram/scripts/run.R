
run.solve <- function(gram) {
    
    len <- length(strsplit(gram, " ")[[1]])
    
    for (n in c(3:1)) {
        n.gram <- character()     
        filename.dtm <- run.makefilename.dtm(n)
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
        
        candidate.list <- run.next.gram.candidates(dtm, n.gram)
        if (length(candidate.list) > 0) {
            break}
        if (n == 1) {
            load("./files/freq.dist.txt.gz")
            candidate.list[[1]] <- 1
            candidate.list <- sample(names(top.dist), 1, prob = top.dist) # if candiate.list doesn't return values, take a random stopword.
        }
    }
    # print(candidate.list)
    candidate.list[ceiling(runif(1, min = 0, max = length(candidate.list)))] #   randomly select if there is a tie
    
}


run.makefilename.dtm <- function(n) {
    modefile <- "50K.blogs.txt"
    filename <- paste(n+1, n, sep = "x")  
    filename <- paste(filename, "dtm", sep = "-")
    filename <- paste(filename, modefile, sep = "_")
    filename <- paste(filename, "gz", sep = ".")
    filename <- paste("./files/", filename, sep="")
    filename
}


run.next.gram.candidates <- function(dtm, gram) {
    candidate.list <- list()
    result = tryCatch({
        pairs <- dimnames(dtm)$Terms[dtm[dtm[, gram]$i,]$j]
    },  error = function(e) {
        return(candidate.list)
    })
    if (is.list(result)) {return(candidate.list)}
    #print(pairs)
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
                # list is now indexed by count -- list[1]$words, list[2]$words, etc.
                # loop backward over this list, when gram is found, add it to the next-highest index value.
                # return the highest-order sublist
                #
                for (j in c(length(candidate.list):1)) {
                    if (listsplit[[1]][listlen] %in% names(candidate.list[[j]])) {
                        # create next level if it doesn't exist
                        if (j == length(candidate.list)) {
                            candidate.list[[j+1]] <- list()  
                        }
                        candidate.list[[j+1]][[listsplit[[1]][listlen]]] = 1
                        break
                    }
                    if (j == 1) {
                        candidate.list[[j]][[listsplit[[1]][listlen]]] = 1  
                    }
                    
                }
            }
        }
    }
    names(candidate.list[[length(candidate.list)]])
}
