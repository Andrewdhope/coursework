# course_week: 4.1 
# description: plotting exercise with R plot functions
# make generic: No

plot4 <- function(download = FALSE) {
  if(download == TRUE) {
    grab("data", "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", "household_power_consumption.zip", read = FALSE, unzip = TRUE)
  }
  # get data
  tenrows <- read.table("data/household_power_consumption.txt", header = TRUE, sep = ";", nrows = 10)
  classes <- sapply(tenrows, class)
  fulldata <- read.table("data/household_power_consumption.txt", header = TRUE, sep = ";", colClasses = classes, na.strings = "?")
  
  # trim down to the needed date range
  library(lubridate)
  daterange <- fulldata[((dmy(fulldata$Date) == "2007-02-01") | (dmy(fulldata$Date) == "2007-02-02")),]
  # return(daterange)
  
  # create a datetime column
  library(plyr)
  mutated <- mutate(daterange, datetime = dmy_hms(paste(daterange$Date, daterange$Time)))
  
  # start plot
  png(file = "plot4.png")
  par(mfrow = c(2,2), mfcol = c(2, 2))
  with(mutated, plot(mutated$datetime, mutated$Global_active_power, xlab = "", ylab = "Global Active Power", type = "l"))
  plot(mutated$datetime, mutated$Sub_metering_1, type = "l", xlab = "", ylab = "Energy sub metering")
  lines(mutated$datetime, mutated$Sub_metering_2, col = "red")
  lines(mutated$datetime, mutated$Sub_metering_3, col = "blue")
  legend(x = "topright", legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col=c("black", "red", "blue"), lty = "solid")
  plot(mutated$datetime, mutated$Voltage, xlab = "datetime", ylab = "Voltage", type = "l")
  plot(mutated$datetime, mutated$Global_reactive_power, xlab = "datetime", ylab = "Global_reactive_power", type = "l")
  dev.off()
}

grab <- function(folder, url, filename, read = TRUE, unzip = FALSE) {
  path <- paste("./", folder, sep = "")
  if(!file.exists(path)) {
      dir.create(path)
  }
  filepath <- paste(path, "/", filename, sep = "")
  download.file(url, destfile = filepath)
  if(unzip == TRUE) {
    unzip(filepath, exdir = path)
  }
  if(read == TRUE) {
      df <- read.csv(filepath)
      return(df)
  }
}