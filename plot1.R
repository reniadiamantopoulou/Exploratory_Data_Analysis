################################################################################

#Data Science Specialization, Coursera
#     Exploratory Data Analysis - Course Project 2

# Created by Renia Diamantopoulou
#11 September 2016



################################################################################
################################################################################
##### plot1.R#####
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
head(NEI)
################################################################################

##Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
##Using the base plotting system,load make a plot showing the total PM2.5 
##emission from all sources for each of the years 1999, 2002, 2005, and 2008.
aggEmissions <- aggregate(Emissions ~ year,NEI, sum)

png("plot1.png",width=480,height=480)
barplot( 
        (aggEmissions$Emissions)/10^6,col = "cyan",
        names.arg=aggEmissions$year,
        xlab="Year",
        ylab="PM2.5 Emissions (10^6 Tons)",
        main=" Total PM2.5 emission from all sources" )
dev.off()
################################################################################
