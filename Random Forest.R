setwd('~/Documents/TAMIDS Taxi Data')

library(data.table)
library(dplyr)
library(magrittr)
library(MASS)
library(gridExtra)
library(randomForest)
library(mlr)

taxi_2013 <- fread('subset_2013.csv')
taxi_2014 <- fread('subset_2014.csv')
taxi_2015 <- fread('subset_2015.csv')
taxi_2016 <- fread('subset_2016.csv')
taxi_2017 <- fread('subset_2017.csv')

taxi_2013[, 12 := NULL]
names(taxi_2014) <- names(taxi_2013)
names(taxi_2015) <- names(taxi_2013)
names(taxi_2016) <- names(taxi_2013)
names(taxi_2017) <- names(taxi_2013)

taxi_df <- data.table(rbindlist(list(taxi_2013, taxi_2014, taxi_2015, taxi_2016)),
                      Year = rep(c(2013, 2014, 2015, 2016), times = c(nrow(taxi_2013), nrow(taxi_2014), nrow(taxi_2015), nrow(taxi_2016))))
taxi_df <- na.omit(taxi_df)
taxi_2017 <- na.omit(taxi_2017)
date_time <- strptime(taxi_df$`Trip Start Timestamp`, '%m/%d/%Y %I:%M:%S %p')
date_time_2017 <- strptime(taxi_2017$`Trip Start Timestamp`, '%m/%d/%Y %I:%M:%S %p')

## Training and testing data
# Create training data
taxi_train <- taxi_df[, .(Year = as.factor(Year),
                        Month = as.numeric(strftime(date_time, '%m')),
                        Hour = as.numeric(strftime(date_time, '%H')),
                        Day = as.numeric(strftime(date_time, '%d')),
                        Region = as.factor(`Pickup Community Area`))][, .(Pickups = .N), by = c('Year', 'Region', 'Month', 'Day', 'Hour')]

taxi_train[, `:=`(Month_sin = sin(Month),
                Month_cos = cos(Month),
                Hour_sin = sin(Hour),
                Hour_cos = cos(Hour),
                Day_sin = sin(Day),
                Day_cos = cos(Day))]

y_train <- taxi_train$Pickups

# Create testing data
taxi_test <- taxi_2017[, .(Year = as.factor(rep(2017, nrow(taxi_2017))),
                           Month = as.numeric(strftime(date_time_2017, '%m')),
                           Hour = as.numeric(strftime(date_time_2017, '%H')),
                           Day = as.numeric(strftime(date_time_2017, '%d')),
                           Region = as.factor(`Pickup Community Area`))][, .(Pickups = .N), by = c('Year', 'Region', 'Month', 'Day', 'Hour')]

taxi_test[, `:=`(Month_sin = sin(Month),
                 Month_cos = cos(Month),
                 Hour_sin = sin(Hour),
                 Hour_cos = cos(Hour),
                 Day_sin = sin(Day),
                 Day_cos = cos(Day))]

y_test <- taxi_test$Pickups

## Tuning random forest parameters
# Optimize ntree and nodesize -- ntree = 419, nodesize = 495
traintask <- makeClassifTask(data = as.data.frame(taxi_train), target = 'Pickups')

rf.lrn <- makeLearner("classif.randomForest")
params <- makeParamSet(makeIntegerParam("ntree",lower = 1,upper = 600), makeIntegerParam("nodesize",lower = 100,upper = 1000))
rdesc <- makeResampleDesc("CV",iters=5L)
ctrl <- makeTuneControlRandom(maxit = 5L)
tune <- tuneParams(learner = rf.lrn, task = traintask, resampling = rdesc, measures = list(acc), par.set = params, control = ctrl, show.info = T)

# Optimize mtry -- found that mtry = 4 is best for speed performances
tuneRF(taxi_dta, y_train)

# Run Random Forest model -- 11.3 min runtime
set.seed(6969)
start_time <- Sys.time()
rf <- randomForest(taxi_train, y_train, mtry = 4, ntree = 400, nodesize = 500)
end_time <- Sys.time()
end_time - start_time

print(rf)
# MSE = 0.168
# % Var explained = 98.96

p1 <- predict(rf, taxi_test)






