# Install ggplot and ggmap (this is only required once)
install.packages('leaflet', dependencies = TRUE)
install.packages('maps', dependencies = TRUE)
install.packages('sp', dependencies = TRUE)
install.packages('rgdal', dependencies = TRUE)
install.packages('rgeos', dependencies = TRUE)

# Load Librarys
library(leaflet)
library(maps)
library(sp)
library(rgdal)
library(rgeos)

#read in environmental justice blocks
ej_blocks <- readOGR("ej2010", "EJ_POLY")
ej_blocks <- spTransform(ej_blocks, CRS("+proj=longlat +datum=WGS84"))
View(ej_blocks)

#select block designated as low income
income_blocks <- ej_blocks[ej_blocks$INCOME == "I" & !is.na(ej_blocks$INCOME), ]

pal <- colorFactor(palette = c("#E69F00"), domain = ej_blocks$INCOME)

#make leaflet map
access_map = leaflet() %>% addTiles() %>%
  addPolygons(data=income_blocks, weight=1, color = ~pal(INCOME), fillOpacity = 1)

access_map

# Read in MBTA Station csv file from Lesson 1
station_locs <- read.csv(file="./mbta_stops.csv", head=TRUE,sep=",")

# Convert the longitude and latitue to spatial coordinates
coordinates(station_locs) <- ~longitude + latitude

# Add a buffer of a 1/4 mile around the point
walking_distance_buffer <- gBuffer(station_locs, width=0.004, byid = TRUE)

# Add proejction WGS84
proj4string(walking_distance_buffer) <- "+proj=longlat +datum=WGS84"

# make leaflet map
access_map =  leaflet(station_locs) %>%
  addTiles() %>%
  addPolygons(data=income_blocks, weight=1, color = ~pal(INCOME), fillOpacity = 1)  %>%
  addCircleMarkers(radius=1, stroke=1, color="#0072B2") %>%
  addPolygons(data=walking_distance_buffer, weight=1, color = "#0072B2", stroke = FALSE, fillOpacity = 0.5)

access_map

# Add Legend
access_map = access_map %>% addLegend('bottomleft',
                                      colors =c("#E69F00","#0072B2"),
                                      labels= c("Low Income Blocks","1/4 mile Buffer"),
                                      title= "Subway Access to Low Income Blocks",
                                      opacity = 1)
access_map
 