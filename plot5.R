################################################################################

#Data Science Specialization, Coursera
#     Exploratory Data Analysis - Course Project 2

# Created by Renia Diamantopoulou
#11 September 2016

################################################################################
################################################################################
##### plot5.R#####
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
head(NEI)
###############################################################################
#How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
library(ggplot2)

vehicles <- grepl("vehicle", SCC$SCC.Level.Two, ignore.case=TRUE)
SCC.vehicles <- SCC[vehicles,]$SCC
NEI.vehicles <- NEI[NEI$SCC %in% SCC.vehicles,]


Baltimore_Vehicles_NEI <- subset(NEI.vehicles,fips=='24510')
Baltimore_Vehicles_NEI.df <- aggregate(Baltimore_Vehicles_NEI[, 'Emissions'], by=list(Baltimore_Vehicles_NEI$year), sum)
colnames(Baltimore_Vehicles_NEI.df) <- c('year', 'Emissions')

png("plot5.png",width=480,height=480)
ggplot(Baltimore_Vehicles_NEI.df,aes(factor(year),Emissions)) +
        geom_bar(stat="identity",fill=Baltimore_Vehicles_NEI.df$year,width=0.75) +
        theme_bw() +  guides(fill=FALSE) +
        labs(x="year", y=expression("Total PM"[2.5]*" Emission (10^5 Tons)")) + 
        labs(title=expression("PM"[2.5]*" Motor Vehicle Source Emissions in Baltimore from 1999-2008"))+
        geom_text(aes(label=round(Emissions,0), size=1, hjust=0.5, vjust=2))+
        theme(legend.position='none')
dev.off()

################################################################################