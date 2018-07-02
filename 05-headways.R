# Install ggplot and lubridate (this is only required once)
install.packages('ggplot2', dependencies = TRUE)
install.packages('lubridate', dependencies = TRUE)

# Load ggplot into current session
library(ggplot2)

# Install lubridate
library(lubridate)

# Read in MBTA headway csv data file
rawbustimes <- read.csv(file="./m3_rawdata-2017-10.csv", head=TRUE,sep=",")

# Create a working dataframe
bustimes <- rawbustimes
View(bustimes)

# Remove any empty columns 
bustimes <- bustimes[, colSums(is.na(bustimes)) != nrow(bustimes)]

# Convert time to posxict with UTC time
bustimes$time <- as.POSIXct(bustimes$time, origin="1970-01-01", tz = "UTC")

# Convert time to the local time
localtime <- with_tz(bustimes$time, "America/New_York")
bustimes <- cbind(localtime,bustimes)

# Remove the UTC time column
bustimes <- within(bustimes, rm(time))

# Create new data frame with just the M3 stop_404120
# UNION SQ E/E 15 ST Stopcode 404120
# This is a north bound train
headways <- bustimes[, c('localtime','stop_404120')]
View(headways)

# The index increases incrementally every time a bus is at or approacing at a stop
# Add 1 to make sure the index doesn't start at zero
headways$index <- cumsum(headways$stop_404120)+1

# Add 1 to the start of the index, and shift the rest of the index down a row
headways$index <- c(1, headways$index[1:length(headways$index) - 1])

# Get the date from index.
headways$lastbus <- c(headways$localtime[1], headways[which(headways$stop_404120==1), "localtime"])[headways$index]

# Find the difference (in seconds) of the time of the recording and the time of the last bus found
# Multiply by stop_404120 to isolate the time intervals, only if there was a bus at the time of the recording.
headways$headway = difftime(headways$localtime, headways$lastbus, units="secs")
headways$headway <- headways$headway * headways$stop_404120

View(headways)

# Remove the first stop time
headways <- with(headways, headways[index != 1 , ])

# Remove any lastbus from the previous day
headways <- with(headways, headways[wday(localtime) == wday(lastbus) , ])

# Select times 7am EDT through 10am EDT using the with function from lubridate
# The different between ET and UTC is 4 hours, we want the time to after 11:00 UTC and before 14:00pm UTC.  
headways <- with(headways, headways[hour(localtime) >= 7 &  hour(localtime) < 14 , ] )

# select data monday through friday using the with function from lubridate
headways <- with(headways, headways[wday(localtime) >= 2 & wday(localtime) <= 6 , ] )
View(headways)

# Calculate the average headway. Remove any 0 and null values from the calculation.
headways$headway <- as.numeric(headways$headway)
headway_mean <- mean(headways$headway[headways$headway != 0], na.rm=TRUE)/60

# Identify the different days
# Convert to Date object (Remove the time and just keep the date)
headways$day <- as.Date(headways$localtime, tz = "America/New_York")
headways <- with(headways, headways[stop_404120 != 0 , ])

# Calcuate the average headway per weekday
headway_mean_by_day <- aggregate(x = headways, by=list(headways$day), FUN=mean, na.rm=TRUE)
headway_mean_by_day$headway_mean <- headway_mean_by_day$headway/60
View(headway_mean_by_day)

# Plot
# Make Subway Ridership Linegraph
# Initialize a ggplot using railrides dataframe, and define axes 
colnames(headway_mean_by_day)[7] <- "DAY"
colnames(headway_mean_by_day)[8] <- "HEADWAY"
theme_set(theme_classic())
headwaysgraph <- ggplot(headway_mean_by_day, aes(x=DAY, y=HEADWAY))+geom_bar(fill=rgb(0.9,0.6,0), stat="identity")
headwaysgraph <- headwaysgraph + ggtitle("MBTA M3 Average Headways for October 2017")
plot(headwaysgraph)


