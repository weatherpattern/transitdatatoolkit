# Install ggplot and lubridate (this is only required once)
install.packages('ggplot2', dependencies = TRUE)
install.packages('lubridate', dependencies = TRUE)
install.packages('data.table', dependencies = TRUE)

# Load ggplot into current session
library(ggplot2)
# Install lubridate
library(lubridate)
# Load data.table
library(data.table)

# Read in MTA SF Survey Results
raw_survey_2017 <- read.csv(file="./TDS_202017_20Data-WEBPAGE.csv", head=TRUE,sep=",")
# Create a working dataframe
survey_2017 <- raw_survey_2017
View(survey_2017)

# Select the columns for Rideshare useage, age, and income
rideshare <- survey_2017[c("Q21A","Q27","Q29")]

# Keep only the resposents with missing answers
rideshare <- rideshare[complete.cases(rideshare), ]

# Rename columns to meaningful names.
colnames(rideshare)[1] = "USAGE"
colnames(rideshare)[2] = "AGE"
colnames(rideshare)[3] = "INCOME"
View(rideshare)

# Separate the rows into age groups
rideshare$AGE_RANGE <- cut(rideshare$AGE,
                           breaks = 7, 
                           labels = c("18-24 yrs","25-34 yrs", "35-44 yrs", "45-54 yrs", "55-64 yrs", "65+ yrs", "NA"), 
                           right = FALSE)

# Separate the rows into income groups
rideshare$INCOME_RANGE <- cut(rideshare$INCOME,
                              breaks = 8, 
                              labels = c("$15,000 or less","$15001-$25,000", "$25001-$35,000", "$35001-$75,000", 
                                         "$75001-$100,000", "$100,001-$200,000", "Over $200,000", "NA"), 
                              right = FALSE)

# Reformat the usage score coding to go from increasing ridership
rideshare$USAGE_SCORE <- 0
rideshare$USAGE_SCORE <- ((rideshare$USAGE == 2) * 5) + rideshare$USAGE_SCORE
rideshare$USAGE_SCORE <- ((rideshare$USAGE == 3) * 4) + rideshare$USAGE_SCORE
rideshare$USAGE_SCORE <- ((rideshare$USAGE == 4) * 3) + rideshare$USAGE_SCORE
rideshare$USAGE_SCORE <- ((rideshare$USAGE == 5) * 2) + rideshare$USAGE_SCORE
rideshare$USAGE_SCORE <- ((rideshare$USAGE == 6) * 1) + rideshare$USAGE_SCORE


# Find average usage score by age range
rideshare_mean_byage <- aggregate(USAGE_SCORE~AGE,mean, data=rideshare)
rideshare_mean_byage$USAGE_SCORE <-  round(rideshare_mean_byage$USAGE_SCORE, digits = 2)
rideshare_mean_byage$AGE <- cut(rideshare_mean_byage$AGE,
                           breaks = 7, 
                           labels = c("18-24 yrs","25-34 yrs", "35-44 yrs", "45-54 yrs", "55-64 yrs", "65+ yrs", "NA"), 
                           right = FALSE)

# Find average usage score by income range
rideshare_mean_byincome <- aggregate(USAGE_SCORE~INCOME,mean, data=rideshare)
rideshare_mean_byincome$USAGE_SCORE <- round(rideshare_mean_byincome$USAGE_SCORE, digits = 2)
rideshare_mean_byincome$INCOME <- cut(rideshare_mean_byincome$INCOME,
                              breaks = 8, 
                              labels = c("$15,000 or less","$15001-$25,000", "$25001-$35,000", "$35001-$75,000", 
                                         "$75001-$100,000", "$100,001-$200,000", "Over $200,000", "NA"), 
                              right = FALSE)

# Plot usage score by age
usagebyage <- ggplot(rideshare_mean_byage, aes(x=AGE, y=USAGE_SCORE, label=USAGE_SCORE)) + 
  geom_point(stat='identity', fill='black', size=10)  +
  geom_segment(aes(y = 0, 
                   x = AGE, 
                   yend = USAGE_SCORE, 
                   xend = AGE), 
               color = "black") +
  geom_text(color="white", size=3) +
  labs(title="Rideshare Usage by Age", 
       subtitle="SFMTA Transit Decision Survey") + 
  ylim(0, 6) +
  coord_flip()
plot(usagebyage)

# Plot usage score by income
usagebyincome <- ggplot(rideshare_mean_byincome, aes(x=INCOME, y=USAGE_SCORE, label=USAGE_SCORE)) + 
  geom_point(stat='identity', fill='black', size=10)  +
  geom_segment(aes(y = 0, 
                   x = INCOME, 
                   yend = USAGE_SCORE, 
                   xend = INCOME), 
               color = "black") +
  geom_text(color="white", size=3) +
  labs(title="Rideshare Usage by Income", 
       subtitle="SFMTA Transit Decision Survey") + 
  ylim(0, 6) +
  coord_flip()
plot(usagebyincome)



# Find the mean by age and income
rideshare_mean <- aggregate(USAGE2~AGE+INCOME,mean, data=rideshare)

#fixed bins
bins <- ggplot(rideshare, aes(age)) + scale_fill_brewer(palette = "Spectral")
bins + geom_bar(aes(fill=USAGE), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Categorical Variable", 
       subtitle="Manufacturer across Vehicle Classes") 

# Scatterplot
gg <- ggplot(rideshare_mean, aes(AGE2, INCOME2))
gg + geom_point(aes(size=USAGE2)) +
  labs(subtitle="AGE vs INCOME", 
       y="Income", 
       x="Age", 
       title="Usage of Ridesharing by Age and Income", 
       caption = "Source: SFfghfgh")

plot(gg)

data("midwest")
midwest
usageplot <- ggplot(rideshare, aes(AGE))
usageplot + geom_density(aes(fill=factor(USAGE)), alpha=0.8) + 
  labs(title="Density plot", 
       subtitle="City Mileage Grouped by Number of cylinders",
       caption="Source: mpg",
       x="AGE",
       fill="USAGE")
chart



rideshare$AGE2 <- cut(rideshare$AGE,
                      breaks = c(1, 2, 3, 4, 5, 6, 7, Inf), 
                      labels = c("18-24 yrs","25-34 yrs", "35-44 yrs", "45-54 yrs", "55-64 yrs", "65+ yrs", "NA"), 
                      right = FALSE)

# Find the mean by income
rideshare_mean$INCOME_RANGE <- cut(rideshare_mean$INCOME,
                                   breaks = 8, 
                                   labels = c("$15,000 or less","$15001-$25,000", "$25001-$35,000", "$35001-$75,000", 
                                              "$75001-$100,000", "$100,001-$200,000", "Over $200,000", "NA"), 
                                   right = FALSE)

# Find the mean by age
rideshare_mean$AGE_RANGE <- cut(rideshare_mean$AGE,
                                breaks = 7, 
                                labels = c("18-24 yrs","25-34 yrs", "35-44 yrs", "45-54 yrs", "55-64 yrs", "65+ yrs", "NA"), 
                                right = FALSE)