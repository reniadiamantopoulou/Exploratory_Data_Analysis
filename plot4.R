################################################################################

#Data Science Specialization, Coursera
#     Exploratory Data Analysis - Course Project 2

# Created by Renia Diamantopoulou
#11 September 2016


################################################################################
################################################################################
##### plot4.R#####
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
head(NEI)
###############################################################################
#Across the United States, how have emissions from coal 
#combustion-related sources changed from 1999–2008?
library(ggplot2)

SCC.coal = SCC[grepl("coal", SCC$Short.Name, ignore.case=TRUE),]

# Merge two data sets
merge <- merge(x=NEI, y=SCC.coal, by='SCC')
merge.total <- aggregate(merge[, 'Emissions'], by=list(merge$year), sum)
colnames(merge.total) <- c('Year', 'Emissions')

png("plot4.png",width=480,height=480)

ggplot(data=merge.total, aes(x=Year, y=Emissions/1000)) + 
        geom_line(aes(group=1, col=Emissions)) + geom_point(aes(size=1, col=Emissions))+
        ggtitle(expression('Total Emissions from coal combustion-related sources')) + 
        ylab(expression(paste('Emissions from coal sources in Kilotons'))) + 
        geom_text(aes(label=round(Emissions/1000,digits=1), size=1, hjust=0.5, vjust=1.5)) + 
        theme(legend.position='none')

dev.off()
################################################################################