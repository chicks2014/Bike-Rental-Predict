#########################################################################################################
# Load libraries
#########################################################################################################
rm(list=ls())
# set working directory
setwd("C:/Users/chetan_hirapara/Downloads/Chicks/Learning/Edwisor_main/Projects/BikeRental")

# verify working directory
getwd()

# load libraries
libraryList <- c(
  "ggplot2",
  "corrgram",
  "sqldf"
)
lapply(libraryList, require, character.only = TRUE)

## load data
bike_rental_train <- read.csv("day.csv", header = T, na.strings = c(" ", "", "NA", NA))

# check dimention of data frame
dim(bike_rental_train)

# remove unique id column
bike_rental_train <- subset(bike_rental_train, select = -c(instant, casual, registered))

# view structure of data
str(bike_rental_train)

# convert some variables from int to factor
for (i in seq(2,8)) {
  print(i)
  bike_rental_train[,i] <- as.factor(bike_rental_train[,i])
}

# view summary of dtaa
summary(bike_rental_train)

# print column names
colnames(bike_rental_train)

#-------------------------------
# EDA
# ------------------------------

# check bike counts group season and weekday
season_summary_by_weekday <- sqldf('select season, weekday, avg(cnt) as count from bike_rental_train group by season, weekday')

# From this plot it shows, 
# There are more rental on working days
# People rent bikes more in Fall, and much less in Spring
ggplot(bike_rental_train, aes(x=weekday, y=count, color=season))+geom_point(data = season_summary_by_weekday, aes(group = season))+geom_line(data = season_summary_by_weekday, aes(group = season))+ggtitle("Bikes rental count by season")+scale_colour_hue('Season',breaks = levels(bike_rental_train$season), labels=c('spring', 'summer', 'fall', 'winter')) + scale_y_continuous("Count", pretty_breaks(n = 25))

# Get the average count of bikes rent by weather, hour
weather_summary_by_weekday <- sqldf('select weathersit, weekday, avg(cnt) as count from bike_rental_train group by weathersit, weekday')

# From this plot it shows, 
# People rent bikes more when weather is good
# We can see lowest bike rent count is on sunday when weather is very bad
ggplot(bike_rental_train, aes(x=weekday, y=count, color=weathersit))+geom_point(data = weather_summary_by_weekday, aes(group = weathersit))+geom_line(data = weather_summary_by_weekday, aes(group = weathersit))+ggtitle("Bikes rental count by weather")+ scale_colour_hue('Weather',breaks = levels(bike_rental_train$weathersit), labels=c('Good', 'Normal', 'Bad')) + scale_y_continuous("Count", pretty_breaks(n = 25))


# Get the average count of bikes rent by weather, hour
weather_summary_by_month <- sqldf('select mnth, weekday, avg(cnt) as count from bike_rental_train group by mnth, weekday')

# From this plot it shows, 
# People rent bikes more in June to Sept months
# We can see lowest bike rent count is on sunday of Dec month 
ggplot(bike_rental_train, aes(x=weekday, y=count, color=mnth))+geom_point(data = weather_summary_by_month, aes(group = mnth))+geom_line(data = weather_summary_by_month, aes(group = mnth))+ggtitle("Bikes rental count by month")+ scale_colour_hue('Month',breaks = levels(bike_rental_train$mnth), labels=c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')) + scale_y_continuous("Count", pretty_breaks(n = 25))

# --------
# Temp Vs Count
ggplot(bike_rental_train,aes(temp,cnt)) + 
  geom_point(aes(color=temp),alpha=0.2) + theme_bw() + xlab("Temperature") + ylab("Count") + labs(title = "Temperature Vs Count")

# Humidity Vs Count
ggplot(bike_rental_train, aes(hum, cnt)) + geom_point(aes(color = season)) + 
  scale_x_continuous("Humidity", pretty_breaks(n = 10))+
  scale_y_continuous("Count", pretty_breaks(n = 10))+ 
  theme_bw() + labs(title="Humidity Vs Count for all seasons") + facet_wrap( ~ season) +
  scale_colour_hue('Season',breaks = levels(bike_rental_train$season), labels=c('spring', 'summer', 'fall', 'winter'))

# Temp vs count
ggplot(bike_rental_train, aes(temp, cnt)) + geom_point(aes(color = season)) + 
  scale_x_continuous("Temperture", pretty_breaks(n = 10))+
  scale_y_continuous("Count", pretty_breaks(n = 10))+ 
  theme_bw() + labs(title="Temperature Vs Count for all seasons") + facet_wrap( ~ season) +
  scale_colour_hue('Season',breaks = levels(bike_rental_train$season), labels=c('spring', 'summer', 'fall', 'winter'))

# Windspeed vs count
ggplot(bike_rental_train, aes(windspeed, cnt)) + geom_point(aes(color = season)) + 
  scale_x_continuous("Wind speed", pretty_breaks(n = 10))+
  scale_y_continuous("Count", pretty_breaks(n = 10))+ 
  theme_bw() + labs(title="Windspeed Vs Count for all seasons") + facet_wrap( ~ season) +
  scale_colour_hue('Season',breaks = levels(bike_rental_train$season), labels=c('spring', 'summer', 'fall', 'winter'))

# weekday vs count
ggplot(bike_rental_train,aes(weekday,cnt)) + 
  geom_point(aes(color=weekday),alpha=0.8) + 
  theme_bw() +
  scale_colour_hue('Weekday',breaks = levels(bike_rental_train$weekday), labels=c('Sun', 'Mon', 'Tue', 'Wed','Thur', 'Fri', 'Sat')) +
  labs(title = "Weekday Vs Count")

# weather vs count
ggplot(bike_rental_train,aes(weathersit,cnt)) + 
  geom_point(aes(color=weathersit),alpha=0.8) + 
  theme_bw() +
  scale_colour_hue('Weather',breaks = levels(bike_rental_train$weathersit), labels=c('Good', 'Normal', 'Bad')) +
  labs(title = "Weather Vs Count") +
  xlab("Weather") + ylab("Count")

# --------------------
# Missing values
# -------------------

# check missing values count
sum(is.na(bike_rental_train))

# -------------------
# Outlier Analysis
# -------------------

# draw box plot
for (i in 1:length(numericColNames)) {
  assign(paste0("san", i), ggplot(aes_string(y = (numericColNames[i]), x = "target"), data = subset(santanderTrain)) + stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour = "red", fill = "gray", outlier.shape = 18, outlier.size = 1, notch = FALSE) +
           theme(legend.position = "bottom") +
           labs(y = numericColNames[i], x = "target") +
           ggtitle(paste("Box plot of target for", numericColNames[i])))
}


## plotting plots together for few variables
gridExtra::grid.arrange(san1, san2, san3, san4, san5, san6, san7, san8, san9, san10, ncol = 10)
gridExtra::grid.arrange(san11, san12, ncol = 2)



