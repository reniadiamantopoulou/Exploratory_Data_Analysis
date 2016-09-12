################################################################################

#Data Science Specialization, Coursera
#     Exploratory Data Analysis - Course Project 2

# Created by Renia Diamantopoulou
#11 September 2016



################################################################################
################################################################################
##### plot6.R#####
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
head(NEI)
###############################################################################
#Compare emissions from motor vehicle sources in Baltimore City with emissions 
#from motor vehicle sources in Los Angeles County, California (𝚏𝚒𝚙𝚜 == "𝟶𝟼𝟶𝟹𝟽
# "). Which city has seen greater changes over time in motor vehicle emissions?
library(ggplot2)

Baltimore_Vehicles_NEI <- subset(NEI.vehicles,fips=='24510')
Baltimore_Vehicles_NEI$city <- "Baltimore City"

LA_Vehicles_NEI <- subset(NEI.vehicles,fips=="06037")
LA_Vehicles_NEI$city <- "Los Angeles County"

both_NEI <- rbind(Baltimore_Vehicles_NEI,LA_Vehicles_NEI)

png("plot6.png",width=800,height=500)
ggplot(both_NEI, aes(x=factor(year), y=Emissions, fill=city)) +
        geom_bar(aes(fill=year),stat="identity") +
        facet_grid(scales="free", space="free", .~city) +
        guides(fill=FALSE) + theme_bw() +
        labs(x="year", y=expression("Total PM"[2.5]*" Emission (KiloTons)")) + 
        labs(title=expression("PM"[2.5]*" Motor Vehicle Source Emissions in Baltimore & LA, 1999-2008"))

dev.off()