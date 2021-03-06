# course_week: 2.4
# description: read file, filter by variable parameter, sort by second parameter, make df of best of sorted variable
# make generic: yes
best <- function(state, outcome){
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClass = "character") # set all of the columns as character vectors
  switch(outcome, 
         "heart attack" = outcomeCol <- 11,
         "heart failure" = outcomeCol <- 17,
         "pneumonia" = outcomeCol <- 23, stop("invalid outcome"))
  stateSelector <- outcomeData$State == state # take the state column
  outcomeState <- outcomeData[stateSelector, ]
  outcomeState[, outcomeCol] <- as.numeric(outcomeState[, outcomeCol]) # convert to numeric
  complete <- complete.cases(outcomeState[, outcomeCol]) 
  outcomeStateComplete <- outcomeState[complete, ]
  sortedOutcome <- sort(outcomeStateComplete[, outcomeCol]) # sort by the outcome column
  minHospitals <- outcomeStateComplete[, outcomeCol] == sortedOutcome[1] # take the minimum value of the sorted column
  hospitalNames <- outcomeStateComplete[minHospitals, 2] # make a data frame of the best row(s)
  hospitalNames
}

# course_week: 2.4
# description: read file, filter by variable parameter, order by second variable, return name of row in third-variable place
# make generic: yes
rankhospital <- function(state, outcome, num) {
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClass = "character")
  switch(outcome, 
         "heart attack" = outcomeCol <- 11,
         "heart failure" = outcomeCol <- 17,
         "pneumonia" = outcomeCol <- 23, stop("invalid outcome"))
  stateSelector <- outcomeData$State == state  # logical vector by state
  outcomeState <- outcomeData[stateSelector, ]
  outcomeState[, outcomeCol] <- as.numeric(outcomeState[, outcomeCol]) # coerce outcome column to numeric
  complete <- complete.cases(outcomeState[, outcomeCol])
  outcomeStateComplete <- outcomeState[complete, ]
  orderedOutcome <- outcomeStateComplete[order(outcomeStateComplete[,outcomeCol], outcomeStateComplete[, 2]),]
  if (num == "best") {num <- 1}
  if (num == "worst")  {num <- nrow(orderedOutcome)}
  hospitalName <- orderedOutcome[num, 2]
  hospitalName
}

# course_week: 2.4
# description: read file, filter by variable parameter, split, take the second-variable place in each partition, return df of results
# make generic: yes
rankall <- function(outcome, num = "best") {
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClass = "character")
  switch(outcome, 
         "heart attack" = outcomeCol <- 11,
         "heart failure" = outcomeCol <- 17,
         "pneumonia" = outcomeCol <- 23, stop("invalid outcome"))
  if (num == "worst") {worst <- TRUE}
  else {worst <- FALSE}
  colData <- as.numeric(outcomeData[, outcomeCol])
  colDataComplete <- complete.cases(colData)
  outcomeDataComplete <- outcomeData[colDataComplete, ]
  outcomeDataComplete[, outcomeCol] <- as.numeric(outcomeDataComplete[, outcomeCol])
  # browser()
  states <- split(outcomeDataComplete, outcomeDataComplete$State)
  for (i in states) {
      orderedOutcome <- i[order(i[outcomeCol], i[2]),]
      if (num == "best") {num <- 1}
      if (worst)  {num <- nrow(orderedOutcome)}
      binder <- data.frame("hospital" = orderedOutcome[num, 2], "state" = orderedOutcome[1, 7])
      hospitalNames <- rbind(hospitalNames, binder)
  }
  hospitalNames
}