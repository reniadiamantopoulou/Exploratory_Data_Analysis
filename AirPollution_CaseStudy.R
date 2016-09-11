pollution_2012 <- read.csv("daily_88101_2012.csv" , header = TRUE , sep = "," ,na.strings = "",as.is= TRUE,
                       col.names = c("State Code" ,"County Code",
                                     "Site Num","Parameter Code","POC","Latitude",
                                     "Longitude","Datum","Parameter Name",
                                     "Sample Duration","Pollutant Standard",
                                     "Date Local","Units of Measure","Event Type",
                                     "Observation Count","Observation Percent","Arithmetic Mean",
                                     "1st Max Value","1st Max Hour","AQI","Method Code",
                                     "Method Name","Local Site Name","Address",
                                     "State Name","County Name","City Name","CBSA Name","Date of Last Change"))

dim(pollution_1999)
x0 <- pollution_1999$Arithmetic.Mean
x1 <- pollution_2012$Arithmetic.Mean
class(x0)
str(x0)
summary(x0)
summary(x1) ### good first comparison
mean(is.na(x0)) ###how many (proportion) of missing values
boxplot(x0,x1) ### visual representation
boxplot(log10(x0),log10(x1)) ### even the boxplots check the spread

negative <- x1 <0 ###logical vector to grap the negative values
str(negative)  ###  logi [1:296208] FALSE FALSE FALSE FALSE FALSE FALSE
sum(negative, na.rm = TRUE)  ###how many negative values
mean(negative, na.rm = TRUE)  ###proportion of negative values in file

dates_1999 <- as.Date(pollution_1999$Date.Local)
str(dates_1999)
dates_2012 <- pollution_2012$Date.Local #####if you include as.is= TRUE in read.csv then the dates are read as strings and not factos (easier)
dates_2012_a <- as.Date(dates_2012)
hist(dates_2012_a, "month")  ###histogram by month
hist(dates_2012_a[negative], "month") ###histogram by month for the negative values

###pick one monitor/location exists both in 1999 and 2012 , pick NY state find 
### a minitor to see the change of the levesl##
### subset the data to take all the monitors for NY state
site1999 <- unique(subset(pollution_1999 ,State.Code ==36, c(County.Code, Site.Num)))
site2012 <- unique(subset(pollution_2012, State.Code ==36, c(County.Code, Site.Num)))
head(site1999)
head(site2012)

###county.code AND site.num paste together
site1999 <- paste(site1999[,1], site1999[,2], sep = ".")
site2012 <- paste(site2012[,1], site2012[,2], sep = ".")
str(site1999)
str(site2012)

### find monitors that both appear in 1999 and 2012 data
both <- intersect(site1999, site2012)
### both
###[1] "1.5"     "1.12"    "5.80"    "5.110"   "13.11"   "29.5"    "31.3"    "63.2008"
###[9] "67.1015" "85.55"   "101.3"  
### 10 monitors/counties that are present in both
### we want the monitor that has a lot of observations in it to compare
### how may observations are in each of these monitors in each of this time periods
###at first create a new column called County.Site
pollution_1999$County.Site <- with (pollution_1999, paste(County.Code,Site.Num,sep = "."))
pollution_2012$County.Site <- with (pollution_2012, paste(County.Code,Site.Num,sep = "."))
#### subset this data frame for NY state (state = 36) and for these common monitors
cnt1999 <- subset(pollution_1999, State.Code ==36 & County.Site %in% both)
cnt2012 <- subset(pollution_2012, State.Code ==36 & County.Site %in% both)
head(cnt1999)
#### split by monitor, separate data frames by each common monitor
sapply(split(cnt1999, cnt1999$County.Site), nrow)
sapply(split(cnt2012, cnt2012$County.Site), nrow)
#### returns the number of observations for each date frame

#### most observations for both for county 63. 2008
####sapply(split(cnt1999, cnt1999$County.Site), nrow)
####1.12     1.5   101.3   13.11    29.5    31.3   5.110    5.80 63.2008 67.1015 
####28      54     108      40      48     104      88      34      92      84 
####85.55 
####5 
####> 
####        > sapply(split(cnt2012, cnt2012$County.Site), nrow)
####1.12     1.5   101.3   13.11    29.5    31.3   5.110    5.80 63.2008 67.1015 
####105     227     112     113     118      51      59      50      97     115 
####85.55 
####108 
#### returns the number of observations for each date frame

###we decide to see what is happening with 63.2008 monitor (County.Code = 63, Site.Num = 2008)
        
pollution_1999sub <- subset(pollution_1999, State.Code == 36 & County.Code == 63 & Site.Num ==2008 )
pollution_2012sub <- subset(pollution_2012, State.Code == 36 & County.Code == 63 & Site.Num ==2008 )

dim(pollution_1999sub)

#####now lets see chronologicaly, x axis = date , y axis = pollution
dates_1999 <- as.Date(pollution_1999sub$Date.Local) ## get the dates
data_1999 <- pollution_1999sub$Arithmetic.Mean ##get tghe values
dates_2012 <- as.Date(pollution_2012sub$Date.Local) ## get the dates
data_2012 <- pollution_2012sub$Arithmetic.Mean ##get the values

par(mfrow = c(1,2), mar = c(5,4,1,1))
plot(dates_1999,data_1999)
abline(h = median(data_1999, na.rm = T)) ##add line for median
plot(dates_2012, data_2012)
abline(h = median(data_2012, na.rm = T))
## put plots in the same range
## check the range
rng <- range(data_1999, data_2012, na.rm = T)
par(mfrow = c(1,2), mar = c(5,4,1,1))
plot(dates_1999,data_1999, ylim = rng)
abline(h = median(data_1999, na.rm = T)) ##add line for median
plot(dates_2012, data_2012, ylim = rng)
abline(h = median(data_2012, na.rm = T))

####### look at individual states


#############################################################
##to calculate the average we use tapply, takes the mean of a vector within subgroups
mn_1999 <- with(pollution_1999, tapply(Arithmetic.Mean, State.Code, mean, na.rm = T))
mn_2012 <- with(pollution_2012, tapply(Arithmetic.Mean, State.Code, mean, na.rm = T))
##create data frame for the means
df_1999 <- data.frame(state = names(mn_1999), mean =mn_1999)
df_2012 <- data.frame(state = names(mn_2012), mean =mn_2012)
####merge the 2 data frames
mrg <- merge(df_1999, df_2012, by = "state")
dim(mrg)
head(mrg)

par (mfrow = c(1,1))
with(mrg, plot(rep(1999,51), mrg[,2], xlim = c(1998, 2013)))
with(mrg, points(rep(2012,51), mrg[,3]))
##connect the dots
segments(rep(1999, 52), mrg[,2], rep(2012, 52), mrg[,3])


############################################################################
#####mean(is.na(x0)) if on summary NA values appear
#####or  if you want you could do 
####pollution_2012 <- read.table("daily_88101_2012.csv", header = false,sep = ",", na.strings = "")
####cnames <- readlines("daily_88101_2012.csv",1) ####creates a string as one line
#### cnames <-strsplit(cnames, ",", fixed = TRUE) #### split string with fixed pattern returns list
#### names(pollution_2012) <- cnames[[1]]
#### names(pollution_2012) <- make.names(names[[1]])
