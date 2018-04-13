## Random forest pickup
setwd('~/Documents/TAMIDS Taxi Data')

library(data.table)
library(dplyr)
library(magrittr)
library(MASS)
library(ggplot2)
library(gridExtra)

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

taxi_2013 <- na.omit(taxi_2013)
taxi_2014 <- na.omit(taxi_2014)
taxi_2015 <- na.omit(taxi_2015)
taxi_2016 <- na.omit(taxi_2016)
taxi_2017 <- na.omit(taxi_2017)

date_time_2013 <- strptime(taxi_2013$`Trip Start Timestamp`, format = '%m/%d/%Y %I:%M:%S %p')
date_time_2014 <- strptime(taxi_2014$`Trip Start Timestamp`, format = '%m/%d/%Y %I:%M:%S %p')
date_time_2015 <- strptime(taxi_2015$`Trip Start Timestamp`, format = '%m/%d/%Y %I:%M:%S %p')
date_time_2016 <- strptime(taxi_2016$`Trip Start Timestamp`, format = '%m/%d/%Y %I:%M:%S %p')
date_time_2017 <- strptime(taxi_2017$`Trip Start Timestamp`, format = '%m/%d/%Y %I:%M:%S %p')

# 2013
t13 <- taxi_2013[, .(Month = as.numeric(strftime(date_time_2013, '%m')),
                     Hour = as.numeric(strftime(date_time_2013, '%H')),
                     Day = as.numeric(strftime(date_time_2013, '%d')),
                     Latitude = `Pickup Centroid Latitude`,
                     Longitude = `Pickup Centroid Longitude`)][, .(Pickups = .N), by = list(Month, Hour, Day, Latitude, Longitude)]
t13[, `:=`(Month_sin = sin(Month),
           Month_cos = cos(Month),
           Hour_sin = sin(Hour),
           Hour_cos = cos(Hour),
           Day_sin = sin(Day),
           Day_cos = cos(Day))]

# 2014
t14 <- taxi_2014[, .(Month = as.numeric(strftime(date_time_2014, '%m')),
                     Hour = as.numeric(strftime(date_time_2014, '%H')),
                     Day = as.numeric(strftime(date_time_2014, '%d')),
                     Latitude = `Pickup Centroid Latitude`,
                     Longitude = `Pickup Centroid Longitude`)][, .(Pickups = .N), by = list(Month, Hour, Day, Latitude, Longitude)]
t14[, `:=`(Month_sin = sin(Month),
           Month_cos = cos(Month),
           Hour_sin = sin(Hour),
           Hour_cos = cos(Hour),
           Day_sin = sin(Day),
           Day_cos = cos(Day))]

# 2015
t15 <- taxi_2015[, .(Month = as.numeric(strftime(date_time_2015, '%m')),
                     Hour = as.numeric(strftime(date_time_2015, '%H')),
                     Day = as.numeric(strftime(date_time_2015, '%d')),
                     Latitude = `Pickup Centroid Latitude`,
                     Longitude = `Pickup Centroid Longitude`)][, .(Pickups = .N), by = list(Month, Hour, Day, Latitude, Longitude)]
t15[, `:=`(Month_sin = sin(Month),
           Month_cos = cos(Month),
           Hour_sin = sin(Hour),
           Hour_cos = cos(Hour),
           Day_sin = sin(Day),
           Day_cos = cos(Day))]

# 2016
t16 <- taxi_2016[, .(Month = as.numeric(strftime(date_time_2016, '%m')),
                     Hour = as.numeric(strftime(date_time_2016, '%H')),
                     Day = as.numeric(strftime(date_time_2016, '%d')),
                     Latitude = `Pickup Centroid Latitude`,
                     Longitude = `Pickup Centroid Longitude`)][, .(Pickups = .N), by = list(Month, Hour, Day, Latitude, Longitude)]
t16[, `:=`(Month_sin = sin(Month),
           Month_cos = cos(Month),
           Hour_sin = sin(Hour),
           Hour_cos = cos(Hour),
           Day_sin = sin(Day),
           Day_cos = cos(Day))]

# 2017
t17 <- taxi_2017[, .(Month = as.numeric(strftime(date_time_2017, '%m')),
                     Hour = as.numeric(strftime(date_time_2017, '%H')),
                     Day = as.numeric(strftime(date_time_2017, '%d')),
                     Latitude = `Pickup Centroid Latitude`,
                     Longitude = `Pickup Centroid Longitude`)][, .(Pickups = .N), by = list(Month, Hour, Day, Latitude, Longitude)]
t17[, `:=`(Month_sin = sin(Month),
           Month_cos = cos(Month),
           Hour_sin = sin(Hour),
           Hour_cos = cos(Hour),
           Day_sin = sin(Day),
           Day_cos = cos(Day))]

taxi_df <- data.table(rbindlist(list(t13, t14, t15, t16)))




