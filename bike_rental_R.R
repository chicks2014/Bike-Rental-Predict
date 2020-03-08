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
  "xgboost",
  "scales",
  "usdm"
)
lapply(libraryList, require, character.only = TRUE)

## load data
bike_rental <- read.csv("day.csv", header = T, na.strings = c(" ", "", "NA", NA))

# -----------------------------
# Exploratory data analysis
# -----------------------------

# First let denormalize our dataset for EDA
# for temp and atemp we'll use below formula 
# y^ = y^ norm× * (maxY−minY) + minY
# for temp given values are
#  t_min=-8, t_max=+39
bike_rental$temp = (bike_rental$temp * (39 - (-8))) + (-8)


# for atemp given values are
# t_min=-16, t_max=+50
bike_rental$atemp = (bike_rental$atemp * (50 - (-16))) + (-16)


# for hum given values was divided by 100
# So, we can calculate actual values by multiply with 100
bike_rental$hum = bike_rental$hum * 100

# for windspeed given values was divided by 67
# So, we can calculate actual values by multiply with 67
bike_rental$windspeed = bike_rental$windspeed * 67

# check dimention of data frame
dim(bike_rental)

# view summary of data
summary(bike_rental)

# top 5 records from casual, registered and cnt
bike_rental$casual[0:5] + bike_rental$registered[0:5] == bike_rental$cnt[0:5]

# It seems that cnt variable is addition of casual and registered variable
# We can remove casual and registered variable
# remove unnecessary variables like instant and dteday which are not fruiful for model 
bike_rental <- subset(bike_rental, select = -c(instant, dteday , casual, registered))

# view structure of data
str(bike_rental)

# convert some variables from int to factor for perform EDA
for (i in seq(1, 7)) {
  bike_rental[, i] <- as.factor(bike_rental[, i])
}

# view summary of dtaa
summary(bike_rental)

# print column names
colnames(bike_rental)

#---------------------------------------------------------- 
# Bivariant analysis
#---------------------------------------------------------- 

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

#----------------------------------------------------------
# Univariant analysis 
#---------------------------------------------------------- 
  
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

# In our dataset we don't have any missing values
# So, need to do further process for missing values

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

# convert factor to numeric
for(i in seq(1,7)) {
  bike_rental[,i] <- as.numeric(bike_rental[,i])
}

# correlation plot
corrgram(bike_rental[,], order = TRUE, lower.panel = panel.shade, upper = panel.pie,  main="Correlation plot")

# correlation between temp and atemp
# It seems that correlation between temp and atemp is 0.9917
# which is too much high
cor(bike_rental$temp,bike_rental$atemp)

# correlation between hum and atemp
cor(bike_rental$hum,bike_rental$temp)

# correlation between temp and windspeed
cor(bike_rental$temp,bike_rental$windspeed)

# correlation between month and season
cor(bike_rental$mnth,bike_rental$season)

# find VIF values for correlation analysis
vifstep(bike_rental, th = 10)

# we can see that there are correlation between temp and atemp
# we can remove atemp variable
bike_rental = subset(bike_rental, select = c(-atemp))

# remove this
# workingday
# season | month

# ---------------------------
# Features scaling
# ---------------------------

# Normalize data
# Now. before moving to build model we need to rescale features
# for temp given values are
#  t_min=-8, t_max=+39
bike_rental$temp = (bike_rental$temp - (-8)) / (39 - (-8)) 

# for hum divide values by 100
bike_rental$hum = bike_rental$hum / 100

# for windspeed divide values by 67
bike_rental$windspeed = bike_rental$windspeed / 67

# ---------------------------------------------------------------------------------------------
# Model development
# ---------------------------------------------------------------------------------------------

# Mean absolute percent error function
mape = function(act,preval) {
  mean(abs((act - preval)/act)) * 100
}

# Mean absolute error function
mae = function(act,preval) {
  mean(abs((act - preval)))
}

# Splitting the Train dataset
set.seed(123)
split <- sample.split(bike_rental$cnt, SplitRatio = 0.80)
train <- subset(bike_rental, split == TRUE)
test <- subset(bike_rental, split == FALSE)

# ------------------------------------------------------
# Multiple linear regression
# ------------------------------------------------------

# multiple linear regression
linear_model <- lm(cnt~. , data = train)

# view model summary
summary(linear_model)

# plot Residuals vs Leverage, Normality, Residual vs Fitted
plot(linear_model)

# predict using linear model
linear_predict <- predict(linear_model, newdata = test)

# calculate mape
mape(test[,11],linear_predict)

# calculate mae
mae(test[,11], linear_predict)


# ************************
# linear regression
# MAPE - 18.9705
# MAE = 679.5772
# Residual standard error: 866.5 on 562 degrees of freedom
# Multiple R-squared:  0.7945,	Adjusted R-squared:  0.7908 
# F-statistic: 217.2 on 10 and 562 DF,  p-value: < 2.2e-16
# ************************

# ------------------------------------------------------
# Random forest
# ------------------------------------------------------

# build random forest model
random_model <- randomForest(cnt ~ ., data = test, importance = TRUE, ntree = 125)

random_model <- randomForest(cnt ~ yr + temp + mnth + season + hum + weathersit + windspeed + holiday, data = test, importance = TRUE, ntree = 125)

# view summary of random forest model
summary(random_model)

# plot variable importance plot
varImpPlot(random_model)

# predict new data using random forest model
random_predict <- predict(random_model, newdata =  test)

# calculate mape
mape(test$cnt, random_predict)

# calculate mae 
mae(test$cnt, random_predict)

# view tree structure
getTree(random_model,1,labelVar = TRUE)

print(random_model)

# plot error vs no of trees
plot(random_model)

# ************************
# Random forest #125
# MAPE = 9.7468
# MAE = 281.9358
# ************************

# ------------------------------------------------------
# XGBoost
# ------------------------------------------------------

# create matrix from train data
# X_train <- subset(train, select = -c(cnt))
X_train <- as.matrix(subset(train, select = -c(cnt)))
X_label <- train$cnt

# create matrix from test data
# X_test <- subset(test, select = -c(cnt))
X_test <- as.matrix(subset(test, select = -c(cnt)))
y_label <- test$cnt

#preparing matrix 
dtrain <- xgb.DMatrix(data = X_train,label = X_label) 
dtest <- xgb.DMatrix(data = X_test,label=y_label)

# -------
#default parameters
params <- list(booster = "gbtree", objective = "reg:linear", eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)

# find best iteration value
xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 100, nfold = 5, showsd = T, stratified = T, print.every.n = 10, early.stop.round = 20, maximize = F)
# -------

# build XGBoost model
xgboost_model = xgb.train(data = dtrain,  nround = 29, max_depth = 5,eta = 0.1, subsample = 0.6, 
                          gamma = 5, min_child_weight = 2, colsample_bytree=0.9, nrounds = 100, eval_metric = 'mae')

# plot variable importance 
xgb.importance(feature_names = colnames(X_train), xgboost_model) %>% xgb.plot.importance()

# predict using XGBoost model
xgb_predict <- predict(xgboost_model, dtest)

# calculate mape
mape(test$cnt, xgb_predict)

# calculate mae
mae(test$cnt, xgb_predict)

# ------------------------------
# XGBoost performance matrix 
# mape = 13.0895
# MAE = 412.8454
# ---------------------------

# ***********************************************

