# Install leaflet (this is only required once)
install.packages('leaflet', dependencies = TRUE)

# Load library
library(leaflet)

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
# Lat Long corrdinates from www.latlong.net
mbta_subway <- leaflet(station_locs) %>%
  addTiles() %>%  
  setView(-71.057083, 42.361145, zoom = 12) %>%
  addCircles(~longitude, ~latitude, weight = 3, radius=120, 
                 color="#0073B2", stroke = TRUE, fillOpacity = 0.8) %>% 
  addLegend("bottomleft", colors="#0073B2", labels="Data Source: MBTA Developer Portal", title="MBTA Subway Stations")
  
# show the map
mbta_subway  

