# 1) Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
# 
# 2) Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? 
# Use the base plotting system to make a plot answering this question.
# 
# 3) Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
# Which have seen increases in emissions from 1999-2008? Use the ggplot2 plotting system to make a plot answer this question.
# 
# 4) Across the United States, how have emissions from coal combustion-related sources changed from 1999-2008?
# 
# 5) How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?
# 
# 6) Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
# Which city has seen greater changes over time in motor vehicle emissions?

plot1 <- function() {
# Check for data files in working directory
    if (!(file.exists(path = "./Source_Classification_Code.rds") & file.exists(path = "./summarySCC_PM25.rds"))) {
        return("Data files are not in current working directory. Download data before attempting to plot.")
    }
# Read files
    nei <- readRDS("summarySCC_PM25.rds")
    scc <- readRDS("Source_Classification_Code.rds")
    
# Plot the sum of PM2.5 emissions for each year using base plot 
    yearTotals <- tapply(nei$Emissions, nei$year, sum)
    png(file = "plot1.png")
    plot(unique(nei$year), yearTotals, xlab = "Year", ylab = "Total PM2.5 Emissions")
    dev.off()
    return()
}