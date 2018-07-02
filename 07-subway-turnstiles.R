# Install ggplot (this is only required once)
install.packages('ggplot2', dependencies = TRUE)

# Load ggplot into current session
library(ggplot2)

# Read in MBTA turnstile csv data file
rawturnstile <- read.csv(file="./turnstile_171125.txt", head=TRUE,sep=",")
View(rawturnstile)

# Create a working dataframe
turnstile <- rawturnstile
View(turnstile)

# Rename CA 
colnames(turnstile)[1] <- "BOOTH"

# Keep Only Regular
turnstile <- turnstile[which(turnstile$DESC == "REGULAR" | turnstile$DESC == "RECOVR AUD"),]

# The first argument is the data to be operated on, the second argument is the group, and the last argument applied diff(x) on group.
turnstile$diff <- ave(turnstile$ENTRIES, turnstile$BOOTH, turnstile$SCP, FUN=function(x) c(0, diff(x)))

# Remove negative entries
turnstile <- turnstile[which(turnstile$diff >  0),]
View(turnstile)


# Select Terminals
terminals <- turnstile[which(turnstile$STATION == "34 ST-PENN STA" | turnstile$STATION == "42 ST-PORT AUTH" | turnstile$STATION == "GRD CNTRL-42 ST" | turnstile$STATION == "ATL AV-BARCLAY"),]

# Sum all the entries by day. The first parameter of aggregate defines the subset of data, in this case the diff, STATION, and DATE.
# The second parameter is the dataframe. The third parameter is the summary statistic, in this case the sum of the subset.
# The final parameter defines what to do for missing values. na.rm=TRUE removes missing values. 
daily_entries <- aggregate(cbind(terminals$diff)~terminals$STATION+terminals$DATE, data=terminals, sum, na.rm=TRUE)

colnames(daily_entries)[1] <- "STATION"
colnames(daily_entries)[2] <- "DATE"
colnames(daily_entries)[3] <- "ENTRIES"
View(daily_entries)


# Plot
# Make Subway Ridership Linegraph
# Initialize a ggplot using railrides dataframe, and define axes 
theme_set(theme_classic())
entriesgraph <- ggplot(daily_entries, aes(x=DATE, y=ENTRIES, group=STATION, color=STATION))
plot(entriesgraph)

entriesgraph <- entriesgraph + geom_point() + geom_line() + scale_color_manual(name="LINE",values=c("42 ST-PORT AUTH"=rgb(0,.45,.70),"34 ST-PENN STA"=rgb(0,.60,.50),"GRD CNTRL-42 ST"=rgb(.90,.60,0),"ATL AV-BARCLAY"=rgb(.80,.40,0)))
plot(entriesgraph)

entriesgraph <- entriesgraph + ggtitle("MBTA Subway Daily Turnstile Entries Week of 11-18-2017")
plot(entriesgraph)

