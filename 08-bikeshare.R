# Install lubridate, data.table, and ggplot packages (this is only required once)
install.packages('lubridate', dependencies = TRUE)
y
install.packages('data.table', dependencies = TRUE)
install.packages('ggplot2', dependencies = TRUE)

# Load data.table
library(data.table)

# Load lubridate
library(lubridate)

# Load ggplot and scales
library(ggplot2)
library(scales)

# Load mapping libraries
library(leaflet)
library(maps)
library(sp)
library(rgdal)
library(rgeos)

# Read in Citibike bikeshare csv data file
rawbikedata <- read.csv(file="./bikeshare_nyc_raw.csv", head=TRUE,sep="\t")

# Create a working data frame
bikedata <- rawbikedata

# Remove any rows which the total docks is zero
bikedata <- bikedata[bikedata$tot_docks != 0 ,]
View(bikedata)

# Select data for the week of October 9th (10/16 - 10/22)
bikedata <- with(bikedata, bikedata[mday(date) <= 22 & mday(date) >= 9, ])

# Create a POSIXct date and time variable using available data
bikedata$date <- as.character.Date(bikedata$date)
bikedata$hour <- bikedata$hour + (bikedata$pm * 12) * (bikedata$hour != 12)
bikedata$hour <- sprintf("%02d",bikedata$hour)
bikedata$minute <- sprintf("%02d",bikedata$minute)

bikedata$hour <- paste(bikedata$hour, bikedata$minute, sep=":" )
bikedata$date <- paste(bikedata$date, bikedata$hour, sep="")
bikedata$date <- as.POSIXct(bikedata$date ,format= "%y-%m-%d %H:%M")

# Create a variable which measure how 'full' a bikeshare dock is
# 0 = empty, 1.0 = full
bikedata$avail_ratio <- bikedata$avail_bikes / bikedata$tot_docks

#Remove columns of data we don't need
bikedata <- bikedata[c("dock_id","dock_name","date","avail_bikes","avail_docks", "tot_docks", "avail_ratio","X_lat", "X_long")]

View(bikedata)

# Select times after 6pm ET using the hour function from lubridate
evening_bikedata <- with(bikedata, bikedata[hour(bikedata$date) >= 18 , ] )

# find the mean of the availability ratio and keep the location coordinates
evening_full <- aggregate(evening_bikedata$avail_ratio, by=list(evening_bikedata$dock_name,evening_bikedata$X_lat,evening_bikedata$X_long), FUN=mean)

# change column names
colnames(evening_full) <- c("dock_name",  "latitude","longitude", "avg_available")

# sort by the availability ratio
evening_full <- evening_full[order(evening_full$avg_available), ]  

# to retain the order in plot.
evening_full$dock_name <- factor(evening_full$dock_name, levels = evening_full$dock_name)  
View(evening_full)

# Draw map of docking stations with fullness ration
palette <- colorNumeric( palette = "Blues", domain = (evening_full$avg_available))

#make leaflet map
access_map = leaflet(evening_full) %>% addTiles() %>%
  addCircles(lng = ~longitude, lat = ~latitude, 
  radius = (evening_full$avg_available+0.25)*100, 
  weight=1, 
  color = ~palette(evening_full$avg_available), 
  fillOpacity = 0.8,
  popup = ~dock_name)

access_map

# Add Legend
access_map = access_map %>% addLegend('bottomleft',
                                      pal = palette, values = ~avg_available,
                                      title= "Citibike Docks Perfect Full<br>Evenings 10/2017 ",
                                      opacity = 1)
access_map


#Draw plot
Dwight_VanDyke <- with(bikedata, bikedata[dock_name == "Dwight St & Van Dyke St" , ] )

theme_set(theme_classic())

# Allow Default X Axis Labels
ggplot(Dwight_VanDyke, aes(x=date, y=avail_ratio)) +
  geom_point(col="tomato2", size=1) + 
  labs(title="Dwight St & Van Dyke St Citi Bike Station Available Bicycles and Docks", 
       subtitle="October 9-22, 2017 after 6 PM ET", 
       caption="Source: Open Bus", 
       y="Availability Ratio")

last_plot() + scale_x_datetime(breaks = date_breaks("1 day"))
