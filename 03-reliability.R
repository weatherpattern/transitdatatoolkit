# Install leaflet (this is only required once)
install.packages('leaflet', dependencies = TRUE)

# Load library
library(leaflet)

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
# Map the stations
# Create a ColorBrewer Greens color palette. 
colorpal <- colorNumeric(palette = "Greens", domain = joindata$rely)

# Lat Long corrdinates from www.latlong.net
mbta_subway <- leaflet(joindata) %>%
  addTiles() %>%  
  setView(-71.057083, 42.361145, zoom = 12) %>%
  addCircles(~LONGITUDE, ~LATITUDE, weight = 3, radius=120, 
             color=~colorpal(rely), stroke = TRUE, fillOpacity = 0.8) %>% 
  addLegend("topright", colorpal, values=~rely, title="MBTA Green Line Reliability, Data Source: MBTA Developer Portal")

# Show the map
mbta_subway  
