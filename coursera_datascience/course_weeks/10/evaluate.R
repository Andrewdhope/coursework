source('C:/Users/ahope/Desktop/_MyFiles/repos/coursework/coursera_datascience/course_weeks/10/reader.R')
source('C:/Users/ahope/Desktop/_MyFiles/repos/coursework/coursera_datascience/course_weeks/10/miner.R')
source('C:/Users/ahope/Desktop/_MyFiles/repos/coursework/coursera_datascience/course_weeks/10/solver.R')
source('C:/Users/ahope/Desktop/_MyFiles/repos/coursework/coursera_datascience/course_weeks/10/plotter.R')
source('C:/Users/ahope/Desktop/_MyFiles/repos/coursework/coursera_datascience/course_weeks/10/main.R')
library(tm)

# make 10 sets of 1000 line files
make.files <- function(type) {
    reader.setwd()
    if (type==1) {string <- "blogs"}
    if (type==2) {string <- "tiwtter"}
    if (type==3) {string <- "news"}
    writename <- string
    readname <- paste("en_US", string, "txt", sep = ".")
    # for (i in c(1:10)) {
        # writename <- paste(string, i, sep = "")
        writename <- paste(writename, "txt", sep = ".")
        write.abbrev("en_US.blogs.txt", writename, 1000)
    # }
}

get.lines <- function(type, num = 1) {
    reader.setwd()
    if (type==1) {string <- "blogs"}
    if (type==2) {string <- "tiwtter"}
    if (type==3) {string <- "news"}
    filename <- string
    path <- paste(".","eval",string,sep = "/")
    setwd(path)
    #filename <- paste(string, num, sep = "")
    filename <- paste(filename, "txt", sep = ".")
    lines <- reader.all(filename)
    lines
}

# turn a file into a list of 1000 4-grams
# how? just take a random vector number, and a random piece between 1 and length-3
# but i need to clean it like i cleaned the corpa
clean.convert <- function(lines) {
    corpus <- VCorpus(VectorSource(lines))
    corpus <- clean.corpus2(corpus)
    corpus
}

clean.corpus2 <- function(corpus) {
    corpus <- tm_map(corpus, removePunctuation)
    repair.apostrophe <- content_transformer(function(x) {return (gsub("â€™", "'", x))})
    #remove.hashtags <- content_transformer(function(x) {return (gsub("#", "", x))})
    #remove.at <- content_transformer(function(x) {return})
    corpus <- tm_map(corpus, repair.apostrophe)
    #tm_map(corpus, remove.hashtags)
    #tm_map(corpus, remove.at)
    #just remove all lines with that alpha thing
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus
}

corpus.to.list <- function(corpus) {
    var <- as.list(sapply(corpus, strwrap))
    var
}


create.comparison <- function(list) {
    path <- 'C:/Users/ahope/Desktop/_MyFiles/repos/coursework/coursera_datascience/course_weeks/10/files'
    setwd(path)
    n <- length(list)
    comparison <- vector("list", n)
    # loop over each line in the cleaned corpus list
    for (i in seq_along(list)) {
        rline <- ceiling(runif(1, 0, length(list[[i]]))) # select a random line of text from the list entry
        words <- strsplit(list[[i]][[rline]], " ") # split into a set of words
        if (length(words[[1]]) > 3) {
            rpos <- ceiling(runif(1, 0, length(words[[1]])-3)) # select a position between the start and end-3  of the set
            four.gram <- words[[1]][rpos:(rpos+3)]
            trimws(four.gram)
            three.gram <- words[[1]][rpos:(rpos+2)]
            trimws(three.gram)
            one.gram <- words[[1]][rpos+3]
            n.gram <- character() # new n.gram
            # paste n.gram words into a single string
            for (j in c(1:3)) {
                n.gram <- paste(n.gram, three.gram[j], sep = " ")
            }
            n.gram <- trimws(n.gram)
            print(paste("line:",i), sep = "")
            guess <- solve3(n.gram, 1) # hard-coded for blogs at this point
            if (length(guess) > 0) {
            comparison[[i]][1] = n.gram
            comparison[[i]][2] = one.gram
            comparison[[i]][3] = guess[1] # need a tiebreak if length of guess > 1
            }
            else {print("no guess")}
        }
    }
    comparison
}


grade <- function(comparison) {
    score <- 0
    denom <- 0
    for (i in seq_along(comparison)) {
        if (!is.null(comparison[[i]]) && all(!is.na(comparison[[i]]))) {
            if (comparison[[i]][[2]] == comparison[[i]][[3]]) {
                score = score + 1
                print(i)
            }
            denom = denom + 1
        }
        else {print(paste("NULL or NA:", i, comparison[[i]][[1]], sep=" "))}
    }
    print(paste(score,"/",denom,sep=""))
    value <- round(score/denom, digits = 4)
    print(paste(value,"%", sep = ""))
}

grade.full <- function(list) {
    comparison <- create.comparison(list)    
    grade(comparison)
}