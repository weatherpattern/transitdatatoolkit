# Install ggplot and ggmap (this is only required once)
install.packages('ggplot2', dependencies = TRUE)
install.packages('ggmap', dependencies = TRUE)

# load packages
library(ggplot2)
library(ggmap)

#read in MBTA performance csv file
rawdata <- read.csv(file="./TDashboardData_reliability_20160301-20160331.csv", head=TRUE,sep=",")
View(rawdata)

#Select Peak Service rows for the Green Line
reliability <- rawdata[which (rawdata$PEAK_OFFPEAK_IND =="Peak Service (Weekdays 6:30-9:30AM, 3:30PM-6:30PM)"),]
View(reliability)

#Remove columns of data we don't need
reliability <- reliability[c("SERVICE_DATE","ROUTE_TYPE","STOP","OTP_NUMERATOR","OTP_DENOMINATOR")]

#Select only the Green Line data
reliability <- reliability[which (reliability$ROUTE_TYPE=='Green Line'),]

#Remove and extra spaces
reliability$STOP <- trimws(reliability$STOP,which = c("right"))

# Find the reliability at each station
# Divide the numerator (people who has to wait too long) by the denominator (all riders).
reliability$rely <- reliability$OTP_NUMERATOR / reliability$OTP_DENOMINATOR
# This is the percentage of people who had to wait. 1 - this ratio is the percentage of people who didn't.
reliability$rely <- 1 - reliability$rely
# Round by 4
reliability$rely <- round(reliability$rely, digits=4)
View(reliability)

# Import the MBTA station location data
stations <- read.csv("./mbta_stations.csv")
View(stations)

# Select only the Green Line data
locs <- stations[which (stations$LINE=='GREEN'),]

# Change the name of Station to Stop to match the other dataframe
colnames(locs)[2] <- "STOP" 
View(locs)


# Join the location data to the reliability file
joindata <- merge(x = reliability, y = locs, by ="STOP", all.x=TRUE)

#Select only the columns need
joindata <- joindata[c("STOP","SERVICE_DATE","ROUTE_TYPE","rely","LONGITUDE","LATITUDE")]
x = !duplicated(joindata$STOP)
joindata <- joindata[x ,]
View(joindata)

# Map the Green Line
map_lat <- c(42.481411, 42.21244)
map_lon <- c(-71.484765, -70.794937)
map_bbox <- make_bbox(map_lon,map_lat, f= 0.05)
mbta_map <- get_map(map_bbox, source="google",maptype="hybrid", zoom=12) 
mbta_subway <- ggmap(mbta_map) + geom_point(data=joindata, aes(x=LONGITUDE, y=LATITUDE, color=rely), size=3, shape=15)+ scale_colour_gradientn(colours=c(rgb(1,1,1),rgb(0,0.45,0.70))) 
mbta_subway <- mbta_subway +   labs(title="MBTA Green Line Reliability", 
                                    caption="Data Source: MBTA Developer Portal")
mbta_subway
