#########################################################################################################
# Load libraries
#########################################################################################################
rm(list = ls())

# set working directory
setwd("C:/BikeRental")

# verify working directory
getwd()

# load libraries
libraryList <- c(
  "ggplot2",
  "corrgram",
  "sqldf",
  "reshape2"
)
lapply(libraryList, require, character.only = TRUE)

## load data
bike_rental_train <- read.csv("day.csv", header = T, na.strings = c(" ", "", "NA", NA))

# check dimention of data frame
dim(bike_rental_train)

# remove unique id column
bike_rental_train <- subset(bike_rental_train, select = -c(instant, dteday , casual, registered))

# view structure of data
str(bike_rental_train)

# convert some variables from int to factor
for (i in seq(1, 7)) {
  bike_rental_train[, i] <- as.factor(bike_rental_train[, i])
}

# view summary of dtaa
summary(bike_rental_train)

# print column names
colnames(bike_rental_train)

#-------------------------------
# EDA
# ------------------------------

# labels of weekday
weekday_labels <- c("Sun", "Mon", "Tue", "Wed", "Thur", "Fri", "Sat")

# lebels of seasons
season_labels <- c("spring", "summer", "fall", "winter")

# weather labels
weather_labels <- c("Good", "Normal", "Bad")

# month labels
month_labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# check bike counts group season and weekday
season_summary_by_weekday <- sqldf("select season, weekday, avg(cnt) as count from bike_rental_train group by season, weekday")

# From this plot it shows,
# There are more rental on working days
# People rent bikes more in Fall, and much less in Spring
ggplot(bike_rental_train, aes(x = weekday, y = count, color = season)) + 
  geom_point(data = season_summary_by_weekday, aes(group = season)) + 
  geom_line(data = season_summary_by_weekday, aes(group = season)) + 
  ggtitle("Bikes rental count by season") + 
  scale_colour_hue("Season", breaks = levels(bike_rental_train$season), labels = season_labels) + 
  scale_y_continuous("Count", pretty_breaks(n = 25)) + 
  scale_x_discrete(labels = weekday_labels)

# Get the average count of bikes rent by weather, hour
weather_summary_by_weekday <- sqldf("select weathersit, weekday, avg(cnt) as count from bike_rental_train group by weathersit, weekday")

# From this plot it shows,
# People rent bikes more when weather is good
# We can see lowest bike rent count is on sunday when weather is very bad
ggplot(bike_rental_train, aes(x = weekday, y = count, color = weathersit)) + 
  geom_point(data = weather_summary_by_weekday, aes(group = weathersit)) + 
  geom_line(data = weather_summary_by_weekday, aes(group = weathersit)) + 
  ggtitle("Bikes rental count by weather") + 
  scale_colour_hue("Weather", breaks = levels(bike_rental_train$weathersit), labels = weather_labels) + 
  scale_y_continuous("Count", pretty_breaks(n = 25)) + 
  scale_x_discrete(labels = weekday_labels)

# Get the average count of bikes rent by weather, hour
weather_summary_by_month <- sqldf("select mnth, weekday, avg(cnt) as count from bike_rental_train group by mnth, weekday")

# From this plot it shows,
# People rent bikes more in June to Sept months
# We can see lowest bike rent count is on sunday of Dec month
ggplot(bike_rental_train, aes(x = weekday, y = count, color = mnth)) + 
  geom_point(data = weather_summary_by_month, aes(group = mnth)) + 
  geom_line(data = weather_summary_by_month, aes(group = mnth)) + 
  ggtitle("Bikes rental count by month") + 
  scale_colour_hue("Month", breaks = levels(bike_rental_train$mnth), labels = month_labels) + 
  scale_y_continuous("Count", pretty_breaks(n = 25)) + 
  scale_x_discrete(labels = weekday_labels)

# -------------------
# Scatter plots
# -------------------
# Temp Vs Count
ggplot(bike_rental_train, aes(temp, cnt)) +
  geom_point(aes(color = temp), alpha = 0.2) + theme_bw() + xlab("Temperature") + ylab("Count") + labs(title = "Temperature Vs Count")

# Humidity Vs Count
ggplot(bike_rental_train, aes(hum, cnt)) + geom_point(aes(color = season)) +
  scale_x_continuous("Humidity", pretty_breaks(n = 10)) +
  scale_y_continuous("Count", pretty_breaks(n = 10)) +
  theme_bw() + labs(title = "Humidity Vs Count for all seasons") + facet_wrap(~season) +
  scale_colour_hue("Season", breaks = levels(bike_rental_train$season), labels = season_labels)

# Temp vs count
ggplot(bike_rental_train, aes(temp, cnt)) + geom_point(aes(color = season)) +
  scale_x_continuous("Temperture", pretty_breaks(n = 10)) +
  scale_y_continuous("Count", pretty_breaks(n = 10)) +
  theme_bw() + labs(title = "Temperature Vs Count for all seasons") + facet_wrap(~season) +
  scale_colour_hue("Season", breaks = levels(bike_rental_train$season), labels = season_labels)

# Windspeed vs count
ggplot(bike_rental_train, aes(windspeed, cnt)) + geom_point(aes(color = season)) +
  scale_x_continuous("Wind speed", pretty_breaks(n = 10)) +
  scale_y_continuous("Count", pretty_breaks(n = 10)) +
  theme_bw() + labs(title = "Windspeed Vs Count for all seasons") + facet_wrap(~season) +
  scale_colour_hue("Season", breaks = levels(bike_rental_train$season), labels = season_labels)

# weekday vs count
ggplot(bike_rental_train, aes(weekday, cnt)) +
  geom_point(aes(color = weekday), alpha = 0.8) +
  theme_bw() +
  scale_colour_hue("Weekday", breaks = levels(bike_rental_train$weekday), labels = weekday_labels) + scale_x_discrete(labels = weekday_labels)
  labs(title = "Weekday Vs Count")

# weather vs count
ggplot(bike_rental_train, aes(weathersit, cnt)) +
  geom_point(aes(color = weathersit), alpha = 0.8) +
  theme_bw() +
  scale_colour_hue("Weather", breaks = levels(bike_rental_train$weathersit), labels = weather_labels) + scale_x_discrete(labels = weather_labels)
  labs(title = "Weather Vs Count")

# -------------------
# Distibuation of continous variable
# -------------------
  
# Temperature distribution
ggplot(bike_rental_train, aes(x = temp)) + geom_density(col = 4) + theme_bw() + stat_bin(bins = 30, fill = "lightblue", color = "blue")

# Humidity distribution
ggplot(bike_rental_train, aes(x = hum)) + geom_density(col = 4) + theme_bw() + stat_bin(bins = 30, fill = "lightblue", color = "blue")

# Wind distribution
ggplot(bike_rental_train, aes(x = windspeed)) + geom_density(col = 4) + theme_bw() + stat_bin(bins = 30, fill = "lightblue", color = "blue")

# -------------------
# Distribution of categorical variables
# -------------------

# season distribution
ggplot(bike_rental_train, aes(x = season)) +
  geom_bar(aes(fill = ..count..)) + scale_fill_gradient("Count", low = "light pink", high = "dark blue") +
  labs(title = "Bike count for season", y = "cnt") + labs(x = "Season", y = "Count") + scale_x_discrete(labels = season_labels)

# month distribution
ggplot(bike_rental_train, aes(x = mnth)) +
  geom_bar(aes(fill = ..count..)) + scale_fill_gradient("Count", low = "light pink", high = "dark blue") +
  labs(title = "Bike count for month", y = "cnt") + labs(x = "Months", y = "Count") + scale_x_discrete(labels = month_labels)

# weekday distribution
ggplot(bike_rental_train, aes(x = weekday)) +
  geom_bar(aes(fill = ..count..)) + scale_fill_gradient("Count", low = "light pink", high = "dark blue") +
  labs(title = "Bike count for weekday", y = "cnt") + labs(x = "Weekday", y = "Count") + scale_x_discrete(labels = weekday_labels)

# holiday distribution
ggplot(bike_rental_train, aes(x = holiday)) +
  geom_bar(aes(fill = ..count..)) + scale_fill_gradient("Count", low = "light pink", high = "dark blue") +
  labs(title = "Bike count for holiday", y = "cnt") + labs(x = "Holiday", y = "Count") + scale_x_discrete(labels = c("non-holiday", "holiday"))


# Weather distribution
ggplot(bike_rental_train, aes(x = weathersit)) +
  geom_bar(aes(fill = ..count..)) + scale_fill_gradient("Count", low = "light pink", high = "dark blue") +
  labs(title = "Bike count for weather", y = "cnt") + labs(x = "Weather", y = "Count") + scale_x_discrete(labels = weather_labels)


# --------------------
# Missing values
# -------------------

# check missing values count
check_missing_values <- function(df) {
  sapply(df, function(x) sum(is.na(x)))
}

# call missing value function
check_missing_values(bike_rental_train)

# -------------------
# Outlier Analysis
# -------------------

# numeric column names
numeric_column_Names = c('temp','atemp','hum','windspeed')

# draw box plot
data_melt <- melt(bike_rental_train, id.vars='cnt', measure.vars=c('temp','atemp','hum','windspeed'))
ggplot(data_melt) + geom_boxplot(aes(x=cnt, y=value, color=variable))

# Handle outlier in two ways 
# 1) Remove outliers 
# 2) Assign NA and impute outliers
# we will replace outliers with NA
for (i in numeric_column_Names) {
  temp_val <- bike_rental_train[, i][bike_rental_train[, i] %in% boxplot.stats(bike_rental_train[, i])$out]
  bike_rental_train[, i][bike_rental_train[, i] %in% temp_val] <- NA
  print(i)
  print(paste("count# ", (length(temp_val))))
}

# missing values imputed by mean method
# Mean method for imputation
for (i in numeric_column_Names) {
  print(i)
  bike_rental_train[, i][is.na(bike_rental_train[, i])] <- mean(bike_rental_train[, i], na.rm = T)
}

# call missing value function
check_missing_values(bike_rental_train)
# ------------------
# 
# ------------------
