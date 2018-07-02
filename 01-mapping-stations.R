# Install ggplot and ggmap (this is only required once)
install.packages('ggplot2', dependencies = TRUE)
install.packages('ggmap', dependencies = TRUE)

# load packages
library(ggplot2)
library(ggmap)

# Read in MBTA Station txt file
rawlocs <- read.csv(file="./stops.txt", head=TRUE,sep=",")
View(rawlocs)

# Select columns with MBTA T stations
station_locs <- rawlocs

#Select the columns we want and change columns name to latitude and longitude
station_locs <- station_locs[c("stop_id","stop_name","stop_lat","stop_lon")]
colnames(station_locs)[3] <- "latitude"
colnames(station_locs)[4] <- "longitude"

# Convert the columns imported as a factor to characters
station_locs$stop_id <- as.character(station_locs$stop_id)
station_locs$stop_name <- as.character(station_locs$stop_name)

# Remove all the rows which do not contain numbers
station_locs <- station_locs[!is.na(as.numeric(station_locs$stop_id)), ]

# Convert the Stop ID column into numbers
station_locs$stop_id = as.numeric(station_locs$stop_id)

# Select columns with MBTA T stations
station_locs <- station_locs[which ((station_locs$stop_id >= 70000) & (station_locs$stop_id <= 70279) ),]


# Saint Paul Street Station names are altered to include their line.
# This change is doen to be able to distinguish the two stations 
# named Saint Paul Street on the B and C line.

station_locs$stop_name[station_locs$stop_id == 70141] <-  "Saint Paul Street B Line"
station_locs$stop_name[station_locs$stop_id == 70218] <-  "Saint Paul Street C Line"

# Find the unique lat and lon coordinates
station_locs <- station_locs[!duplicated(station_locs[c("latitude", "longitude")]),]

# Select the rows which do not have Outbound in the text
# Remove string with dash
station_locs$stop_name <- sub("\\-.*","",station_locs$stop_name)

#Remove and extra spaces
station_locs$stop_name <- trimws(station_locs$stop_name,which = c("right"))
View(station_locs)

#export the data to a csv file
write.csv(station_locs, "./mbta_stops.csv")

# Map the stations
mbta_subway <- qmap("boston", source="google",maptype="roadmap", zoom=11) 
mbta_subway <- mbta_subway + geom_point(data=station_locs, aes(x=longitude, y=latitude), color=rgb(0,0.45,0.70,0.7), size=3)
mbta_subway <- mbta_subway +   labs(title="MBTA Subway Stations", 
                                  caption="Data Source: MBTA Developer Portal")
mbta_subway


