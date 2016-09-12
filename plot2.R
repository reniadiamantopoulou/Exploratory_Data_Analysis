################################################################################

#Data Science Specialization, Coursera
#     Exploratory Data Analysis - Course Project 2

# Created by Renia Diamantopoulou
#11 September 2016


################################################################################
################################################################################
##### plot2.R#####
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
head(NEI)
################################################################################
#Have total emissions from PM2.5 decreased in the Baltimore City,
#Maryland (𝚏𝚒𝚙𝚜 == "𝟸𝟺𝟻𝟷𝟶") from 1999 to 2008? Use the base plotting
#system to make a plot answering this question.

Baltimore_NEI <- subset(NEI, fips=='24510')
aggBaltimore <- aggregate(Emissions ~ year, Baltimore_NEI,sum)

png("plot2.png",width=480,height=480)

barplot(
        aggBaltimore$Emissions, col= "blue",
        names.arg=aggBaltimore$year,
        xlab="Year",
        ylab="PM2.5 Emissions (Tons)",
        main="Total PM2.5 emissions from all Baltimore City Sources")

dev.off()
################################################################################