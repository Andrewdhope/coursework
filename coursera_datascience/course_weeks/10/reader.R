# author: andrew hope
# course_week: 10
# description: functions to read lines from a large, simple text file. 
# make generic: no

reader.setwd <- function() {
    setwd("C:/Users/ahope/Desktop/_MyFiles/repos/coursework/coursera_datascience/course_weeks/10/data/final/en_US")
}

write.abbrev <- function(readname, writename, limit) {
    set <- reader.subset(readname, limit)
    fileConn <- file(writename)
    writeLines(set, fileConn)
    close(fileConn)
}

# generic text file reader.
# can take a limit argument, to read a subset of lines from a large file.
reader.subset <- function(filename, limit) {
    n <- 0
    end <- FALSE
    
 con <- file(filename, "rb") # rb for read binary. handles encoding properly.
 line <- readLines(con, 1) # read first line
 l <- 1
 while ( length(line) != 0 ) {
    # operate on the first line
    l <- l + 1
    val <- innerLoop(line) # optional function to limit which lines we read in.
    if (!(l %% 4) > 0) {
        if (n == 0) {set <- line}
        else {
            preset.operation(line) # optional function to pre-process a selected line.
            set <- rbind(set, line, deparse.level = 0)
        }
        n = n+1
    }
    end <- (n >= limit) # breakLoop(line, val, n)
    if (end) {break}
    line <- readLines(con, 1) # read next line
 }
 close(con)
 set
}

preset.operation <- function(line) {
    return
}

innerLoop <- function(line) {
    # pop in a quick function
    # rbinom(1, 1, 0.05)
    return(1)
}

breakLoop <- function(line, val, n) {
    # break conditional

}

# simply read the first n lines.
reader.some <- function(filename, n) {
    con <- file(filename, "rb")
    lines <- readLines(con, n)   
    close(con)
    lines
}

# read all lines.
reader.all <- function(filename) {
    con <- file(filename, "rb")
    lines <- readLines(con)   
    close(con)
    lines
}