## Random forest pickup
setwd('~/Documents/TAMIDS Taxi Data')

library(data.table)

taxi_2013 <- fread('Chicago_taxi_trips2013.csv')
taxi_2014 <- fread('Chicago_taxi_trips2014.csv')
taxi_2015 <- fread('Chicago_taxi_trips2015.csv')
taxi_2016 <- fread('Chicago_taxi_trips2016.csv')
taxi_2017 <- fread('Chicago_taxi_trips2017.csv')

taxi_2013[, 12 := NULL]
cnames <- names(taxi_2013)
names(taxi_2014) <- cnames
names(taxi_2015) <- cnames
names(taxi_2016) <- cnames
names(taxi_2017) <- cnames

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
t13 <- taxi_2013[, .(Year = strftime(date_time_2013, '%Y'),
                     Year_num = (as.numeric(date_time_2013$yday)+1)/365,
                     Month = as.numeric(strftime(date_time_2013, '%m')),
                     Hour = as.numeric(date_time_2013$h),
                     Day = as.numeric(date_time_2013$mday),
                     WDay = as.numeric(date_time_2013$wday),
                     Latitude = `Pickup Centroid Latitude`,
                     Longitude = `Pickup Centroid Longitude`)][, Pickups := as.numeric(.N), by = list(Year, Month, Hour, Day, Latitude, Longitude)]
t13[, `:=`(Month_num = (Day-1)/30, Hour_num = (Hour+1)/24, Day_num = (((WDay-1) %% 7)+(Hour+1)/24)/7)]
t13[, `:=`(Year_sin = sin(Year_num*2*pi),
           Year_cos = cos(Year_num*2*pi),
           Month_sin = sin(Month_num*2*pi),
           Month_cos = cos(Month_num*2*pi),
           Hour_sin = sin(Hour_num*2*pi),
           Hour_cos = cos(Hour_num*2*pi),
           Day_sin = sin(Day_num*2*pi),
           Day_cos = cos(Day_num*2*pi))][, Year := NULL]

write.csv(t13, file = "Chicago_taxi_2013.csv", row.names = FALSE)
# 2014
t14 <- taxi_2014[, .(Year = strftime(date_time_2014, '%Y'),
                     Year_num = (as.numeric(date_time_2014$yday)+1)/365,
                     Month = as.numeric(strftime(date_time_2014, '%m')),
                     Hour = as.numeric(date_time_2014$h),
                     Day = as.numeric(date_time_2014$mday),
                     WDay = as.numeric(date_time_2014$wday),
                     Latitude = `Pickup Centroid Latitude`,
                     Longitude = `Pickup Centroid Longitude`)][, Pickups := as.numeric(.N), by = list(Year, Month, Hour, Day, Latitude, Longitude)]
t14[, `:=`(Month_num = (Day-1)/30, Hour_num = (Hour+1)/24, Day_num = (((WDay-1) %% 7)+(Hour+1)/24)/7)]
t14[, `:=`(Year_sin = sin(Year_num*2*pi),
           Year_cos = cos(Year_num*2*pi),
           Month_sin = sin(Month_num*2*pi),
           Month_cos = cos(Month_num*2*pi),
           Hour_sin = sin(Hour_num*2*pi),
           Hour_cos = cos(Hour_num*2*pi),
           Day_sin = sin(Day_num*2*pi),
           Day_cos = cos(Day_num*2*pi))][, Year := NULL]

write.csv(t14, file = "Chicago_taxi_2014.csv", row.names = FALSE)

# 2015
t15 <- taxi_2015[, .(Year = strftime(date_time_2015, '%Y'),
                     Year_num = (as.numeric(date_time_2015$yday)+1)/365,
                     Month = as.numeric(strftime(date_time_2015, '%m')),
                     Hour = as.numeric(date_time_2015$h),
                     Day = as.numeric(date_time_2015$mday),
                     WDay = as.numeric(date_time_2015$wday),
                     Latitude = `Pickup Centroid Latitude`,
                     Longitude = `Pickup Centroid Longitude`)][, Pickups := as.numeric(.N), by = list(Year, Month, Hour, Day, Latitude, Longitude)]
t15[, `:=`(Month_num = (Day-1)/30, Hour_num = (Hour+1)/24, Day_num = (((WDay-1) %% 7)+(Hour+1)/24)/7)]
t15[, `:=`(Year_sin = sin(Year_num*2*pi),
           Year_cos = cos(Year_num*2*pi),
           Month_sin = sin(Month_num*2*pi),
           Month_cos = cos(Month_num*2*pi),
           Hour_sin = sin(Hour_num*2*pi),
           Hour_cos = cos(Hour_num*2*pi),
           Day_sin = sin(Day_num*2*pi),
           Day_cos = cos(Day_num*2*pi))][, Year := NULL]

write.csv(t15, file = "Chicago_taxi_2015.csv", row.names = FALSE)
# 2016
t16 <- taxi_2016[, .(Year = strftime(date_time_2016, '%Y'),
                     Year_num = (as.numeric(date_time_2016$yday)+1)/366,
                     Month = as.numeric(strftime(date_time_2016, '%m')),
                     Hour = as.numeric(date_time_2016$h),
                     Day = as.numeric(date_time_2016$mday),
                     WDay = as.numeric(date_time_2016$wday),
                     Latitude = `Pickup Centroid Latitude`,
                     Longitude = `Pickup Centroid Longitude`)][, Pickups := as.numeric(.N), by = list(Year, Month, Hour, Day, Latitude, Longitude)]
t16[, `:=`(Month_num = (Day-1)/30, Hour_num = (Hour+1)/24, Day_num = (((WDay-1) %% 7)+(Hour+1)/24)/7)]
t16[, `:=`(Year_sin = sin(Year_num*2*pi),
           Year_cos = cos(Year_num*2*pi),
           Month_sin = sin(Month_num*2*pi),
           Month_cos = cos(Month_num*2*pi),
           Hour_sin = sin(Hour_num*2*pi),
           Hour_cos = cos(Hour_num*2*pi),
           Day_sin = sin(Day_num*2*pi),
           Day_cos = cos(Day_num*2*pi))][, Year := NULL]

write.csv(t16, file = "Chicago_taxi_2016.csv", row.names = FALSE)

# 2017
t17 <- taxi_2017[, .(Year = strftime(date_time_2017, '%Y'),
                     Year_num = (as.numeric(date_time_2017$yday)+1)/365,
                     Month = as.numeric(strftime(date_time_2017, '%m')),
                     Hour = as.numeric(date_time_2017$h),
                     Day = as.numeric(date_time_2017$mday),
                     WDay = as.numeric(date_time_2017$wday),
                     Latitude = `Pickup Centroid Latitude`,
                     Longitude = `Pickup Centroid Longitude`)][, Pickups := as.numeric(.N), by = list(Year, Month, Hour, Day, Latitude, Longitude)]
t17[, `:=`(Month_num = (Day-1)/30, Hour_num = (Hour+1)/24, Day_num = (((WDay-1) %% 7)+(Hour+1)/24)/7)]
t17[, `:=`(Year_sin = sin(Year_num*2*pi),
           Year_cos = cos(Year_num*2*pi),
           Month_sin = sin(Month_num*2*pi),
           Month_cos = cos(Month_num*2*pi),
           Hour_sin = sin(Hour_num*2*pi),
           Hour_cos = cos(Hour_num*2*pi),
           Day_sin = sin(Day_num*2*pi),
           Day_cos = cos(Day_num*2*pi))][, Year := NULL]

taxi_df <- data.table(rbindlist(list(t13, t14, t15, t16)))

write.csv(taxi_df, file = "PickupData.csv", row.names = FALSE)
write.csv(t17, file = "PickupDataTest.csv", row.names = FALSE)

# Create all possible combinations of location and time for August, 2017 - December, 2017

#expand.grid(Days = c(1:30), Months = c(8:12), Hour = c(1:24), Latitude = t17$Latitude, Location = t17$Longitude)

