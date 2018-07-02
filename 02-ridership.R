# Install ggplot (this is only required once)
install.packages('ggplot2', dependencies = TRUE)

# Load ggplot into current session
library(ggplot2)

# Read in MBTA ridership csv file
rawdata <- read.csv(file="./mbta_ridership_2016.csv", head=TRUE,sep=",")
View(rawdata)

# Select Rail (subway) Rows
railrides <- rawdata[which (rawdata$MODE_TYPE =="RAIL"),]

# Select columns for analysis
railrides <- railrides[,c("SERVICE_MONTH", "MODE_TYPE", "AVERAGE_WEEKDAY_RIDERSHIP_COUNT", "ROUTE_OR_LINE")]

# Rename column names
colnames(railrides)[1] <- "Month"
colnames(railrides)[2] <- "Mode"
colnames(railrides)[3] <- "Avg_Weekday_Ridership"
colnames(railrides)[4] <- "Line"

# Convert columns to characters and date data type
tempdata <- sapply(railrides, is.factor)
railrides[tempdata] <- lapply(railrides[tempdata], as.character)
railrides$Month <- as.Date(railrides$Month, "%d-%b-%Y")
View(railrides)

# Make Subway Ridership Linegraph
# Initialize a ggplot using railrides dataframe, and define axes 
subwayridesgraph <- ggplot(railrides, aes(x=Month, y=Avg_Weekday_Ridership, group=Line, color=Line))
plot(subwayridesgraph)

subwayridesgraph <- subwayridesgraph + geom_point() + geom_line() + scale_color_manual(name="LINE",values=c("BLUE LINE"="blue","GREEN LINE"="green","ORANGE LINE"="orange","RED LINE"="red"))
plot(subwayridesgraph)

subwayridesgraph <- subwayridesgraph + ggtitle("Average Weekday MBTA Subway Ridership 2016")
plot(subwayridesgraph)

# Example of create ggplot in one line
# subwayridesgraph <- ggplot(railrides, aes(x=Month, y=Avg_Weekday_Ridership, group=Line, color=Line)) + geom_point() + geom_line() + scale_color_manual(name="LINE",values=c("BLUE LINE"="blue","GREEN LINE"="green","ORANGE LINE"="orange","RED LINE"="red")) + ggtitle("Average Weekday MBTA Subway Ridership 2016")
# plot(subwayridesgraph)
