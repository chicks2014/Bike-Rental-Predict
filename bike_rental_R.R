#########################################################################################################
# Load libraries
#########################################################################################################
rm(list = ls())

# set working directory
setwd("C:/Users/chetan_hirapara/Downloads/Chicks/Learning/Edwisor_main/Projects/BikeRental/Bike-Rental-Predict")

# verify working directory
getwd()

# load libraries
libraryList <- c(
  "ggplot2",
  "corrgram",
  "sqldf",
  "reshape2",
  "DMwR",
  "MASS",
  "car",
  "caTools",
  "randomForest",
  "dplyr",
  "e1071",
  "xgboost"
)
lapply(libraryList, require, character.only = TRUE)

## load data
bike_rental <- read.csv("day.csv", header = T, na.strings = c(" ", "", "NA", NA))

# check dimention of data frame
dim(bike_rental)

# remove unique id column
bike_rental <- subset(bike_rental, select = -c(instant, dteday , casual, registered))

# view structure of data
str(bike_rental)

# convert some variables from int to factor
for (i in seq(1, 7)) {
  bike_rental[, i] <- as.factor(bike_rental[, i])
}

# view summary of dtaa
summary(bike_rental)

# print column names
colnames(bike_rental)

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
season_summary_by_weekday <- sqldf("select season, weekday, avg(cnt) as count from bike_rental group by season, weekday")

# From this plot it shows,
# There are more rental on working days
# People rent bikes more in Fall, and much less in Spring
ggplot(bike_rental, aes(x = weekday, y = count, color = season)) + 
  geom_point(data = season_summary_by_weekday, aes(group = season)) + 
  geom_line(data = season_summary_by_weekday, aes(group = season)) + 
  ggtitle("Bikes rental count by season") + 
  scale_colour_hue("Season", breaks = levels(bike_rental$season), labels = season_labels) + 
  scale_y_continuous("Count", pretty_breaks(n = 25)) + 
  scale_x_discrete(labels = weekday_labels)

# Get the average count of bikes rent by weather, hour
weather_summary_by_weekday <- sqldf("select weathersit, weekday, avg(cnt) as count from bike_rental group by weathersit, weekday")

# From this plot it shows,
# People rent bikes more when weather is good
# We can see lowest bike rent count is on sunday when weather is very bad
ggplot(bike_rental, aes(x = weekday, y = count, color = weathersit)) + 
  geom_point(data = weather_summary_by_weekday, aes(group = weathersit)) + 
  geom_line(data = weather_summary_by_weekday, aes(group = weathersit)) + 
  ggtitle("Bikes rental count by weather") + 
  scale_colour_hue("Weather", breaks = levels(bike_rental$weathersit), labels = weather_labels) + 
  scale_y_continuous("Count", pretty_breaks(n = 25)) + 
  scale_x_discrete(labels = weekday_labels)

# Get the average count of bikes rent by weather, hour
weather_summary_by_month <- sqldf("select mnth, weekday, avg(cnt) as count from bike_rental group by mnth, weekday")

# From this plot it shows,
# People rent bikes more in June to Sept months
# We can see lowest bike rent count is on sunday of Dec month
ggplot(bike_rental, aes(x = weekday, y = count, color = mnth)) + 
  geom_point(data = weather_summary_by_month, aes(group = mnth)) + 
  geom_line(data = weather_summary_by_month, aes(group = mnth)) + 
  ggtitle("Bikes rental count by month") + 
  scale_colour_hue("Month", breaks = levels(bike_rental$mnth), labels = month_labels) + 
  scale_y_continuous("Count", pretty_breaks(n = 25)) + 
  scale_x_discrete(labels = weekday_labels)

# -------------------
# Scatter plots
# -------------------
# Temp Vs Count
ggplot(bike_rental, aes(temp, cnt)) +
  geom_point(aes(color = temp), alpha = 0.2) + theme_bw() + xlab("Temperature") + ylab("Count") + labs(title = "Temperature Vs Count")

# Humidity Vs Count
ggplot(bike_rental, aes(hum, cnt)) + geom_point(aes(color = season)) +
  scale_x_continuous("Humidity", pretty_breaks(n = 10)) +
  scale_y_continuous("Count", pretty_breaks(n = 10)) +
  theme_bw() + labs(title = "Humidity Vs Count for all seasons") + facet_wrap(~season) +
  scale_colour_hue("Season", breaks = levels(bike_rental$season), labels = season_labels)

# Temp vs count
ggplot(bike_rental, aes(temp, cnt)) + geom_point(aes(color = season)) +
  scale_x_continuous("Temperture", pretty_breaks(n = 10)) +
  scale_y_continuous("Count", pretty_breaks(n = 10)) +
  theme_bw() + labs(title = "Temperature Vs Count for all seasons") + facet_wrap(~season) +
  scale_colour_hue("Season", breaks = levels(bike_rental$season), labels = season_labels)

# Windspeed vs count
ggplot(bike_rental, aes(windspeed, cnt)) + geom_point(aes(color = season)) +
  scale_x_continuous("Wind speed", pretty_breaks(n = 10)) +
  scale_y_continuous("Count", pretty_breaks(n = 10)) +
  theme_bw() + labs(title = "Windspeed Vs Count for all seasons") + facet_wrap(~season) +
  scale_colour_hue("Season", breaks = levels(bike_rental$season), labels = season_labels)

# weekday vs count
ggplot(bike_rental, aes(weekday, cnt)) +
  geom_point(aes(color = weekday), alpha = 0.8) +
  theme_bw() +
  scale_colour_hue("Weekday", breaks = levels(bike_rental$weekday), labels = weekday_labels) + scale_x_discrete(labels = weekday_labels)
  labs(title = "Weekday Vs Count")

# weather vs count
ggplot(bike_rental, aes(weathersit, cnt)) +
  geom_point(aes(color = weathersit), alpha = 0.8) +
  theme_bw() +
  scale_colour_hue("Weather", breaks = levels(bike_rental$weathersit), labels = weather_labels) + scale_x_discrete(labels = weather_labels)
  labs(title = "Weather Vs Count")

# -------------------
# Distibuation of continous variable
# -------------------
  
# Temperature distribution
ggplot(bike_rental, aes(x = temp)) + geom_density(col = 4) + theme_bw() + stat_bin(bins = 30, fill = "lightblue", color = "blue")

# Humidity distribution
ggplot(bike_rental, aes(x = hum)) + geom_density(col = 4) + theme_bw() + stat_bin(bins = 30, fill = "lightblue", color = "blue")

# Wind distribution
ggplot(bike_rental, aes(x = windspeed)) + geom_density(col = 4) + theme_bw() + stat_bin(bins = 30, fill = "lightblue", color = "blue")

# -------------------
# Distribution of categorical variables
# -------------------

# season distribution
ggplot(bike_rental, aes(x = season)) +
  geom_bar(aes(fill = ..count..)) + scale_fill_gradient("Count", low = "light pink", high = "dark blue") +
  labs(title = "Bike count for season", y = "cnt") + labs(x = "Season", y = "Count") + scale_x_discrete(labels = season_labels)

# month distribution
ggplot(bike_rental, aes(x = mnth)) +
  geom_bar(aes(fill = ..count..)) + scale_fill_gradient("Count", low = "light pink", high = "dark blue") +
  labs(title = "Bike count for month", y = "cnt") + labs(x = "Months", y = "Count") + scale_x_discrete(labels = month_labels)

# weekday distribution
ggplot(bike_rental, aes(x = weekday)) +
  geom_bar(aes(fill = ..count..)) + scale_fill_gradient("Count", low = "light pink", high = "dark blue") +
  labs(title = "Bike count for weekday", y = "cnt") + labs(x = "Weekday", y = "Count") + scale_x_discrete(labels = weekday_labels)

# holiday distribution
ggplot(bike_rental, aes(x = holiday)) +
  geom_bar(aes(fill = ..count..)) + scale_fill_gradient("Count", low = "light pink", high = "dark blue") +
  labs(title = "Bike count for holiday", y = "cnt") + labs(x = "Holiday", y = "Count") + scale_x_discrete(labels = c("non-holiday", "holiday"))


# Weather distribution
ggplot(bike_rental, aes(x = weathersit)) +
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
check_missing_values(bike_rental)

# -------------------
# Outlier Analysis
# -------------------

# numeric column names
numeric_column_Names = c('temp','atemp','hum','windspeed')

# draw box plot
data_melt <- melt(bike_rental, id.vars='cnt', measure.vars=c('temp','atemp','hum','windspeed'))
ggplot(data_melt) + geom_boxplot(aes(x=cnt, y=value, color=variable))

# Handle outlier in two ways 
# 1) Remove outliers 
# 2) Assign NA and impute outliers
# We will remove outliers because is less than 2%
for (i in numeric_column_Names) {
  temp_val <- bike_rental[, i][bike_rental[, i] %in% boxplot.stats(bike_rental[, i])$out]
  bike_rental = bike_rental[which(!bike_rental[,i] %in% temp_val),]
  print(i)
  print(paste("count# ", (length(temp_val))))
}

# ------------------
# Features selection 
# ------------------

# correlation plot
corrgram(bike_rental[,], order = TRUE, lower.panel = panel.shade, upper = panel.pie,  main="Correlation plot")

# correlation between temp and atemp
cor(bike_rental$temp,bike_rental$atemp)

# correlation between hum and atemp
cor(bike_rental$hum,bike_rental$temp)

# correlation between temp and windspeed
cor(bike_rental$temp,bike_rental$windspeed)

# we can see that there are correlation between temp and atemp
# we can remove atemp variable
bike_rental = subset(bike_rental, select = c(-atemp))

# ---------------------------------------------------------------------------------------------
# use step wise regression
# feature importance

# ---------------------------------------------------------------------------------------------
# Multiple linear regression
# ---------------------------------------------------------------------------------------------
# Splitting the Train dataset

# convert factor to numeric
for(i in seq(1,7)) {
  bike_rental[,i] <- as.numeric(bike_rental[,i])
}

set.seed(123)
split <- sample.split(bike_rental$cnt, SplitRatio = 0.80)
train <- subset(bike_rental, split == TRUE)
test <- subset(bike_rental, split == FALSE)

# multiple linear regression
linear_model <- lm(cnt~., data = train)

summary(linear_model)

plot(linear_model)

linear_predict <- predict(linear_model, newdata = test)

# MAPE function
mape = function(act,preval) {
  mean(abs((act - preval)/act)) * 100
}

# MAE function
mae = function(act,preval) {
  mean(abs((act - preval)))
}

mae(test[,11], linear_predict)

mape(test[,11],linear_predict)

# Alternative method
regr.eval(test[,11],linear_predict, stats = c("mae","mape"))

# ************************
# linear regression
# MAPE - 18.9705
# MAE = 679.5772
# ************************
# --------------

# use step wise regression
stepwise_AIC = step(linear_model, direction = "both")

summary(stepwise_AIC)

step_linear_predict <- predict(stepwise_AIC, newdata = test)

mape(test$cnt, step_linear_predict)

mae(test$cnt, step_linear_predict)

# ************************
# stepwise regression
# MAPE - 19.2120
# MAE - 684.0707
# ************************

# ------------------------------------------------------
# Random forest

random_model <- randomForest(cnt ~ ., data = test, importance = TRUE, ntree = 125)

# random_model <- randomForest(cnt ~ yr + temp + mnth + season + hum + weathersit + windspeed + holiday, data = test, importance = TRUE, ntree = 125)

# summary(random_model)

varImpPlot(random_model)

random_predict <- predict(random_model, newdata =  test)

mape(test$cnt, random_predict)

mae(test$cnt, random_predict)

getTree(random_model,1,labelVar = TRUE)

print(random_model)

plot(random_model)

# ************************
# Random forest #125
# MAPE = 9.7451
# MAE = 281.83
# ************************

# ----------------------------------------------------
# Support Vector Regression

svm_model <- svm(cnt ~., data = train)

print(svm_model)

svm_predict <- predict(svm_model, newdata = test)

mape(test$cnt, svm_predict)

mae(test$cnt, svm_predict)

# ************************
# SVM performance
# MAPE = 13.6745
# MAE = 419.8084
# ************************

# --------------------------------------------------
# XGBoost

X_train <- train %>% select(-c(cnt)) %>% as.matrix()
y_train <- train$cnt

# XGBoost model
dmatrix <- xgb.DMatrix(X_train, label = y_train)

xgboost_model = xgb.train(data = dmatrix, nround = 150, max_depth = 5, eta = 0.1, subsample = 0.9)

xgb.importance(feature_names = colnames(X_train), xgboost_model) %>% xgb.plot.importance()

X_test <- test %>% select(-c(cnt)) %>% as.matrix()
y_test <- test$cnt

xgb_predict <- predict(xgboost_model, X_test)

mape(test$cnt, xgb_predict)

mae(test$cnt, xgb_predict)

# ------------------------------
# XGBoost 
# mape = 13.0761
# MAE = 411.1326
# ---------------------------

