# Install ggplot, lubridate and data.table (this is only required once)
install.packages('ggplot2', dependencies = TRUE)
install.packages('lubridate', dependencies = TRUE)
install.packages('data.table', dependencies = TRUE)
# Load ggplot into current session
library(ggplot2)

# Install lubridate
library(lubridate)

# Load data.table
library(data.table)

# Read in MBTA headway csv data file
rawbustimes <- read.csv(file="./MTA-Bus-Time_.2014-10-08.txt", head=TRUE,sep="\t")
# Create a working dataframe
bustimes <- rawbustimes
View(bustimes)

# Select Buses in route
bustimes <- with(bustimes, bustimes[inferred_phase == "IN_PROGRESS" , ])

#Remove columns of data we don't need
bustimes <- bustimes[c("time_received","vehicle_id","distance_along_trip",
                       "inferred_direction_id", "inferred_route_id", "inferred_trip_id")]

# Select Bronx Buses
bustimes <- bustimes[grep("_BX", bustimes$inferred_route_id), ]
  
# Create a POSIXct date and time variable using available data
#bustimes$time_received <- as.character.Date(bustimes$time_received)
bustimes$time_received <- as.POSIXct(bustimes$time_received ,format= "%Y-%m-%d %H:%M:%S")

# Select Peak time 07:00 EST (11:00 UTC) to 10:00 EST (14:00 UTC)
bustimes <- with(bustimes, bustimes[hour(time_received) >= 11 &  hour(time_received) < 14 , ] )

View(bustimes)


# Create a format our analysis data frame
speed <- bustimes[c("inferred_trip_id","inferred_route_id", "time_received", "distance_along_trip")]
colnames(speed)[4] <- "max_distance"
speed_min_distance <- bustimes[c("inferred_trip_id", "time_received", "distance_along_trip")]
colnames(speed_min_distance)[2] <- "time"
colnames(speed_min_distance)[3] <- "min_distance"

# Convert data frame to data.table and sort by speed and min_speed
speed <- data.table(speed)
speed_min_distance <- data.table(speed_min_distance)

# Select the maximum distance travelled for each trip ID 
speed <- speed[ , .SD[which.max(max_distance)], by = inferred_trip_id]
# Select the minimum distance travelled for each trip ID 
speed_min_distance <- speed_min_distance[ , .SD[which.min(min_distance)], by = inferred_trip_id]

# Join speed and min_speed on inferred_trip_id
setkey(speed, inferred_trip_id)
setkey(speed_min_distance, inferred_trip_id)
speed <- speed[speed_min_distance, nomatch=0]

View(speed)

speed$time_diff <- speed$time_received - speed$time
speed$distance <- speed$max_distance - speed$min_distance

# Remove any rows which the time difference or distance travelled is zero
speed <- speed[speed$time_diff > 0 , ]
speed <- speed[speed$distance > 0 , ]

speed$m_per_sec <- speed$distance / as.numeric(speed$time_diff)

# meters per second to miles per hour = 1:2.23694
speed$mph <- speed$m_per_sec * 2.237
View(speed)

# find of average (mean) speed from all the trips of each individual route 
average_speed <- speed[ , mean(mph), inferred_route_id ]
colnames(average_speed)[2] <- "avg mph"
View(average_speed)

# Plot
# Initialize a ggplot, and then create lines graphs for bus speeds throughout the day for each bus route.
speedplot <- ggplot(speed, aes(x = time_received, y = mph))
plot(speedplot)
speedplot <- speedplot + geom_line() + facet_wrap(~inferred_route_id, ncol = 10) + theme_light()
plot(speedplot)

# Remove any trips with a speed greater than 30 mph
speed <- speed[speed$mph < 30 , ]

# Replot with outliers remoted
# Initialize a ggplot, and then create lines graphs for bus speeds throughout the day for each bus route.
speedplot <- ggplot(speed, aes(x = time_received, y = mph))
plot(speedplot)
speedplot <- speedplot + geom_line() + facet_wrap(~inferred_route_id, ncol = 10) + theme_light()
speedplot <- speedplot + ggtitle("Average Speeds for MTA Bronx Buses from 7am-10am 2014-10-08")
plot(speedplot)
