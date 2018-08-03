
# The goal of this analysis is to answer two questions related to the degree of damage caused by weather events in the United States.
# First, which types of events are most harmful with respect to population health?
# Second, which types of events have the greatest economic consequences?
# To answer these questions, I analyzed a public dataset from the U.S. National Oceanic and Atmospheric Administration's storm database.
# This database tracks characteristics of major storms and weather events in the United States, including when and where they occur,
# as well as estimates of any fatalities, injuries, and property damage.
# The product of this analysis is a list of the five most damaging weather events evaluated by
# fatalities, injuries, estimated property damage, and estimated crop damage.


getDataset <- function(from = "", folder, filename) {
        if (!nchar(from)>0) {
            from <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
        }
        path <- paste("./", folder, sep = "")
        if(!file.exists(path)) {
            dir.create(path)
        }
        filepath <- paste(path, "/", filename, sep = "")
        download.file(from, destfile = filepath)
}


# Doesn't work well with this dataset. Getting an error.
readFile <- function(filepath) {
    # use colClasses to read faster
    initial <- read.csv(filepath, header = TRUE, nrows = 10)
    classes <- sapply(initial, class)
    full <- read.csv(filepath, header = TRUE, colClasses = classes)
    
}

fatal <- function(df, plot = TRUE) {
# check for null values in the FATALITIES column
    na_check <- sum(as.numeric(is.na(df$FATALITIES)))
# no NAs? Good.
# Remove zero values for performance, just to get a set of rows that we can handle. (come back to this assumption later in the analysis)
    nzdf <- subset(df, df$FATALITIES > 0) # tet with this line deleted
# Sum fatalities per event
    totals <- tapply(nzdf$FATALITIES, nzdf$EVTYPE, sum)
# Check for NAs in the totals list
    length(totals[is.na(totals)])
# But we there were no NAs originally, and we already removed zero-values.
# Well, the EVTYPE column is a factor variable that kept all 985 levels from the original dataset.
# tapply attempts to sum over all of the factors, but many of them no longer have values in the subset.
# would have been better to add first, then remove zeros... try this for the next one.

# Remove NAs
    full_totals <- totals[!is.na(totals)]
    
# Take the top ten    
    ordered_totals <- sort(full_totals, decreasing = TRUE)
    
    if (plot == TRUE) {
        # Plot the top ten
        # (need to set the margin settings to get the labels printed. Or maybe use ggplot)
        par(mar = c(9, 4, 4, 2))
        barplot(unlist(ordered_totals[1:10]), las = 2)
    }
    else { return(ordered_totals)}
}

injurious <- function(df, plot = TRUE) {
    # check for null values in the FATALITIES column
    na_check <- sum(as.numeric(is.na(df$INJURIES)))
    # no NAs? Good.
    # Remove zero values for performance, just to get a set of rows that we can handle. (come back to this assumption later in the analysis)
    nzdf <- subset(df, df$INJURIES > 0) # test with this line deleted
    # Sum fatalities per event
    totals <- tapply(nzdf$INJURIES, nzdf$EVTYPE, sum)
    # Check for NAs in the totals list
    length(totals[is.na(totals)])
    # But we there were no NAs originally, and we already removed zero-values.
    # Well, the EVTYPE column is a factor variable that kept all 985 levels from the original dataset.
    # tapply attempts to sum over all of the factors, but many of them no longer have values in the subset.
    # would have been better to add first, then remove zeros... try this for the next one.
    
    # Remove NAs
    full_totals <- totals[!is.na(totals)]
    
    # Take the top ten    
    ordered_totals <- sort(full_totals, decreasing = TRUE)
    
    if (plot == TRUE) {
    # Plot the top ten
    # (need to set the margin settings to get the labels printed. Or maybe use ggplot)
    par(mar = c(11, 4, 4, 2))
    barplot(unlist(ordered_totals[1:10]), las = 2)
    }
    else { return(ordered_totals)}
}

harmful <- function(df) {
    fatal <- fatal(df)
    injurious <- injurious(df)
    b1 <- as.numeric(as.logical(fatal))
    b2 <- as.numeric(as.logical(injurious))
}

property <- function(df, plot = TRUE) {
    # check for null values in the FATALITIES column
    na_check <- sum(as.numeric(is.na(df$PROPDMG)))
    # no NAs? Good.
    # Remove zero values for performance, just to get a set of rows that we can handle. (come back to this assumption later in the analysis)
    nzdf <- subset(df, df$PROPDMG > 0)
    
    # Make a new column to convert PROPDMGEXP into consistent values
    nzdf$PROPDMGMULT = 0
    
    for (i in seq(length(nzdf$REFNUM))) {
        if (is.numeric(nzdf[i, "PROPDMGEXP"]) == FALSE) {
            nzdf[i, "PROPDMGEXP"] = 0   
        }
        else if (toupper(nzdf[i, "PROPDMGEXP"]) == "H") {
            nzdf[i, "PROPDMGEXP"] = 2    
        }
        else if (toupper(nzdf[i, "PROPDMGEXP"]) == "K") {
            nzdf[i, "PROPDMGEXP"] = 3    
        }
        else if (toupper(nzdf[i, "PROPDMGEXP"]) == "M") {
            nzdf[i, "PROPDMGEXP"] = 6    
        }
        else if (toupper(nzdf[i, "PROPDMGEXP"]) == "B") {
            nzdf[i, "PROPDMGEXP"] = 9    
        }
        nzdf[i, "PROPDMGMULT"] = nzdf[i, "PROPDMG"] * 10^as.numeric(as.character(nzdf[i, "PROPDMGEXP"])) # check this
    }

    # Sum damage per event
    totals <- tapply(nzdf$PROPDMGMULT, nzdf$EVTYPE, sum)
    # Check for NAs in the totals list
    length(totals[is.na(totals)])
    # But we there were no NAs originally, and we already removed zero-values.
    # Well, the EVTYPE column is a factor variable that kept all 985 levels from the original dataset.
    # tapply attempts to sum over all of the factors, but many of them no longer have values in the subset.
    # would have been better to add first, then remove zeros... try this for the next one.
    
    # Remove NAs
    full_totals <- totals[!is.na(totals)]
    
    # Take the top ten    
    ordered_totals <- sort(full_totals, decreasing = TRUE)
    
    if (plot == TRUE) {
        # Plot the top ten
        # (need to set the margin settings to get the labels printed. Or maybe use ggplot)
        par(mar = c(11, 4, 4, 2))
        barplot(unlist(ordered_totals[1:10]), las = 2)
    }
    else { return(ordered_totals)}
}


crop <- function(df, plot = TRUE) {
    # check for null values in the FATALITIES column
    na_check <- sum(as.numeric(is.na(df$CROPDMG)))
    # no NAs? Good.
    # Remove zero values for performance, just to get a set of rows that we can handle. (come back to this assumption later in the analysis)
    nzdf <- subset(df, df$CROPDMG > 0)
    
    # Make a new column to convert PROPDMGEXP into consistent values
    nzdf$CROPDMGMULT = 0
    
    for (i in seq(length(nzdf$REFNUM))) {
        if (is.numeric(nzdf[i, "CROPDMGEXP"]) == FALSE) {
            nzdf[i, "CROPDMGEXP"] = 0   
        }
        else if (toupper(nzdf[i, "CROPDMGEXP"]) == "H") {
            nzdf[i, "CROPDMGEXP"] = 2    
        }
        else if (toupper(nzdf[i, "CROPDMGEXP"]) == "K") {
            nzdf[i, "CROPDMGEXP"] = 3    
        }
        else if (toupper(nzdf[i, "CROPDMGEXP"]) == "M") {
            nzdf[i, "CROPDMGEXP"] = 6    
        }
        else if (toupper(nzdf[i, "CROPDMGEXP"]) == "B") {
            nzdf[i, "CROPDMGEXP"] = 9    
        }
        nzdf[i, "CROPDMGMULT"] = nzdf[i, "CROPDMG"] * 10^as.numeric(as.character(nzdf[i, "CROPDMGEXP"]))
    }
    
    # Sum damage per event
    totals <- tapply(nzdf$CROPDMGMULT, nzdf$EVTYPE, sum)
    # Check for NAs in the totals list
    length(totals[is.na(totals)])
    # But we there were no NAs originally, and we already removed zero-values.
    # Well, the EVTYPE column is a factor variable that kept all 985 levels from the original dataset.
    # tapply attempts to sum over all of the factors, but many of them no longer have values in the subset.
    # would have been better to add first, then remove zeros... try this for the next one.
    
    # Remove NAs
    full_totals <- totals[!is.na(totals)]
    
    # Take the top ten    
    ordered_totals <- sort(full_totals, decreasing = TRUE)
    
    if (plot == TRUE) {
        # Plot the top ten
        # (need to set the margin settings to get the labels printed. Or maybe use ggplot)
        par(mar = c(11, 4, 4, 2))
        barplot(unlist(ordered_totals[1:10]), las = 2)
    }
    else { return(ordered_totals)}
}