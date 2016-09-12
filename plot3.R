################################################################################

#Data Science Specialization, Coursera
#     Exploratory Data Analysis - Course Project 2

# Created by Renia Diamantopoulou
#11 September 2016


################################################################################
################################################################################
##### plot3.R#####
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
head(NEI)
###############################################################################
#Of the four types of sources indicated by the 𝚝𝚢𝚙𝚎 (point, nonpoint, onr
#oad, nonroad) variable, which of these four sources have seen decreases 
#in emissions from 1999–2008 for Baltimore City? Which have seen increases
#in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question
library(ggplot2)
Baltimore_NEI$year <- factor(Baltimore_NEI$year, levels=c('1999', '2002', '2005', '2008'))


png('plot3.png', width=800, height=500, units='px')
ggplot(Baltimore_NEI,aes(factor(year),Emissions,fill=type)) +
        geom_bar(stat="identity") +
        theme_bw() + guides(fill=FALSE)+
        facet_grid(.~type,scales = "free",space="free") + 
        labs(x="year", y=expression("Total PM"[2.5]*" Emission (Tons)")) + 
        labs(title=expression("PM"[2.5]*" Emissions, Baltimore City 1999-2008 by Source Type"))

dev.off()
################################################################################

