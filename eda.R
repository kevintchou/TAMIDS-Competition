setwd('~/Documents/TAMIDS Taxi Data')

library(data.table)
library(dplyr)
library(magrittr)
library(MASS)
library(ggplot2)
library(gridExtra)

## Notes
# taxi 2013 has ~ 33.5 million rows
# taxi 2014 has ~ 31 million rows
# taxi 2015 has ~ 27.4 million rows
# taxi 2016 has ~ 19.9 million rows
# taxi 2016 has ~ 7.68 million rows
#
# Currently using a 1% random subset of the full data

taxi_2013 <- fread('subset_2013.csv')
taxi_2014 <- fread('subset_2014.csv')
taxi_2015 <- fread('subset_2015.csv')
taxi_2016 <- fread('subset_2016.csv')
taxi_2017 <- fread('subset_2017.csv')


View(taxi_2013)
View(taxi_2014)
View(taxi_2015)
View(taxi_2016)
View(taxi_2017)

taxi_2013[, 12 := NULL]
names(taxi_2014) <- names(taxi_2013)
names(taxi_2015) <- names(taxi_2013)
names(taxi_2016) <- names(taxi_2013)
names(taxi_2017) <- names(taxi_2013)

taxi_df <- data.table(rbindlist(list(taxi_2013, taxi_2014, taxi_2015, taxi_2016)),
                      Year = rep(c(2013, 2014, 2015, 2016), times = c(nrow(taxi_2013), nrow(taxi_2014), nrow(taxi_2015), nrow(taxi_2016))))

taxi_df <- na.omit(taxi_df)
## Earnings by company
# 2013
g0 <- taxi_df[, .(TripTotal = sum(as.numeric(gsub('[$]', '', `Trip Total`)), na.rm = TRUE)), by = c('Company', 'Year')][order(TripTotal, decreasing = T)][1:50,] %>%
  ggplot(aes(reorder(Company, TripTotal), TripTotal)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  facet_wrap( ~ as.factor(Year)) +
  labs(x = '', y = 'Trip Total', title = 'Total Company Earnings') +
  coord_flip()
  
g1 <- taxi_2013[, .(TripTotal = sum(as.numeric(gsub('[$]', '', `Trip Total`)), na.rm = TRUE)), by = Company][1:10,] %>% 
  ggplot(aes(reorder(Company, TripTotal), TripTotal)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  labs(x = '', y = 'Trip Total', title = '2013') +
  coord_flip()

# 2014
g2 <- taxi_2014[, .(TripTotal = sum(as.numeric(gsub('[$]', '', `Trip Total`)), na.rm = TRUE)), by = Company][1:10,] %>% 
  ggplot(aes(reorder(Company, TripTotal), TripTotal)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  labs(x = '', y = 'Trip Total', title = '2014') +
  coord_flip()

# 2015
g3 <- taxi_2015[, .(TripTotal = sum(as.numeric(gsub('[$]', '', `Trip Total`)), na.rm = TRUE)), by = Company][1:10,] %>% 
  ggplot(aes(reorder(Company, TripTotal), TripTotal)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  labs(x = '', y = 'Trip Total', title = '2015') +
  coord_flip()

# 2016
g4 <- taxi_2016[, .(TripTotal = sum(as.numeric(gsub('[$]', '', `Trip Total`)), na.rm = TRUE)), by = Company][1:10,] %>% 
  ggplot(aes(reorder(Company, TripTotal), TripTotal)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  labs(x = '', y = 'Trip Total', title = '2016') +
  coord_flip()

grid.arrange(g1, g2, g3, g4, nrow = 2)

## Average Fare per Trip for each company
# 2013
g6 <- taxi_2013[, .(AvgFare = mean(as.numeric(gsub('[$]', '', Fare)), na.rm = TRUE)), by = Company][1:10,] %>%
  ggplot(aes(reorder(Company, AvgFare), AvgFare)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  labs(x = '', y = 'Average fare per trip', title = '2013') +
  coord_flip()

# 2014
g7 <- taxi_2014[, .(AvgFare = mean(as.numeric(gsub('[$]', '', Fare)), na.rm = TRUE)), by = Company][1:10,] %>%
  ggplot(aes(reorder(Company, AvgFare), AvgFare)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  labs(x = '', y = 'Average fare per trip', title = '2014') +
  coord_flip()

# 2015
g8 <- taxi_2015[, .(AvgFare = mean(as.numeric(gsub('[$]', '', Fare)), na.rm = TRUE)), by = Company][1:10,] %>%
  ggplot(aes(reorder(Company, AvgFare), AvgFare)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  labs(x = '', y = 'Average fare per trip', title = '2015') +
  coord_flip()

# 2016
g9 <- taxi_2016[, .(AvgFare = mean(as.numeric(gsub('[$]', '', Fare)), na.rm = TRUE)), by = Company][1:10,] %>%
  ggplot(aes(reorder(Company, AvgFare), AvgFare)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  labs(x = '', y = 'Average fare per trip', title = '2016') +
  coord_flip()

grid.arrange(g6, g7, g8, g9, nrow = 2)

## Average Distance covered by each company
# 2013
g11 <- taxi_2013[, .(TripMiles = mean(`Trip Miles`, na.rm = TRUE)), by = Company][1:10,] %>%
  ggplot(aes(reorder(Company, TripMiles), TripMiles)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  labs(x = '', y = 'Average Miles per trip', title = '2013') +
  coord_flip()

# 2014
g12 <- taxi_2014[, .(TripMiles = mean(`Trip Miles`, na.rm = TRUE)), by = Company][1:10,] %>%
  ggplot(aes(reorder(Company, TripMiles), TripMiles)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  labs(x = '', y = 'Average Miles per trip', title = '2014') +
  coord_flip()

# 2015
g13 <- taxi_2015[, .(TripMiles = mean(`Trip Miles`, na.rm = TRUE)), by = Company][1:10,] %>%
  ggplot(aes(reorder(Company, TripMiles), TripMiles)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  labs(x = '', y = 'Average Miles per trip', title = '2015') +
  coord_flip()

# 2016
g14 <- taxi_2016[, .(TripMiles = mean(`Trip Miles`, na.rm = TRUE)), by = Company][1:10,] %>%
  ggplot(aes(reorder(Company, TripMiles), TripMiles)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  labs(x = '', y = 'Average Miles per trip', title = '2016') +
  coord_flip()

grid.arrange(g11, g12, g13, g14, nrow = 2)

## Distance covered v. amount of tips 
# Takes a while for the plots to load
g16 <- taxi_df[, .(Year = Year, Distance = `Trip Miles`, Tip = as.numeric(gsub('[$]', '', Tips)))] %>% 
  ggplot(aes(x = Distance, y = Tip)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap( ~ as.factor(Year)) +
  labs(x = 'Distance (miles)', y = '$ Tips', title = '$ Tips Received v. Distance Covered')

g16

## Trip Duration Analysis
# Density of trip duration
g17 <- taxi_2013[, .(TripDuration = `Trip Seconds`)] %>%
  ggplot(aes(x = TripDuration)) +
  geom_histogram(stat = 'bin', fill = 'steelblue') +
  ylab('Density') +
  labs(x = 'Trip Duration (seconds)', title = '2013') +
  xlim(0, 4000)

g18 <- taxi_2014[, .(TripDuration = `Trip Seconds`)] %>%
  ggplot(aes(x = TripDuration)) +
  geom_histogram(stat = 'bin', fill = 'steelblue') +
  ylab('Density') +
  labs(x = 'Trip Duration (seconds)', title = '2014') +
  xlim(0, 4000)

g19 <- taxi_2015[, .(TripDuration = `Trip Seconds`)] %>%
  ggplot(aes(x = TripDuration)) +
  geom_histogram(stat = 'bin', fill = 'steelblue') +
  ylab('Density') +
  labs(x = 'Trip Duration (seconds)', title = '2015') +
  xlim(0, 4000)

g20 <- taxi_2016[, .(TripDuration = `Trip Seconds`)] %>%
  ggplot(aes(x = TripDuration)) +
  geom_histogram(stat = 'bin', fill = 'steelblue') +
  ylab('Density') +
  labs(x = 'Trip Duration (seconds)', title = '2016') +
  xlim(0, 4000)

grid.arrange(g17, g18, g19, g20, nrow = 2)

# Trip duration over time
date_time_2013 <- strptime(taxi_2013$`Trip Start Timestamp`, format = '%m/%d/%Y %I:%M:%S %p')
date_time_2014 <- strptime(taxi_2014$`Trip Start Timestamp`, format = '%m/%d/%Y %I:%M:%S %p')
date_time_2015 <- strptime(taxi_2015$`Trip Start Timestamp`, format = '%m/%d/%Y %I:%M:%S %p')
date_time_2016 <- strptime(taxi_2016$`Trip Start Timestamp`, format = '%m/%d/%Y %I:%M:%S %p')
date_time <- strptime(taxi_df$`Trip Start Timestamp`, format = '%m/%d/%Y %I:%M:%S %p')
date_time2 <- strptime(taxi_df$`Trip End Timestamp`, format = '%m/%d/%Y %I:%M:%S %p')

## Time breakdown for predictor analysis
## By Month
# Time Duration
g21 <- taxi_df[, .(TripDuration = `Trip Seconds`, Month = strftime(date_time, '%m')), by = Year] %>%
  ggplot(aes(Month, TripDuration)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  facet_wrap(~ as.factor(Year)) +
  labs(x = '', y = 'Trip Duration (Seconds)', title = 'Trip duration by month')

g21

# Time count
g22 <- taxi_df[, .(Month = strftime(date_time, '%m')), by = Year][, .(TripCount = .N), by = c('Year', 'Month')] %>%
  ggplot(aes(Month, TripCount)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  facet_wrap(~ as.factor(Year)) +
  labs(x = 'Month', y = 'Trip Count', title = 'Trip count by month')

g22

# Trip distance
g23 <- taxi_df[, .(TripDistance = `Trip Miles`, Month = strftime(date_time, '%m')), by = Year] %>%
  ggplot(aes(Month, TripDistance)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  facet_wrap(~ as.factor(Year)) +
  labs(x = 'Month', y = 'Distance', title = 'Distance travelled by month')

g23

## By Day of the week
# Time Duration
g24 <- taxi_df[, .(TripDuration = `Trip Seconds`,
                   Day = factor(weekdays(as.Date(date_time)), levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))), by = Year] %>%
  ggplot(aes(Day, TripDuration)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  facet_wrap(~ as.factor(Year)) +
  labs(x = '', y = 'Trip Duration (seconds)', title = 'Trip duration by day of week')

g24

# Trip count
g25 <- taxi_df[, .(Day = factor(weekdays(as.Date(date_time)), levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))), by = Year][, .(TripCount = .N), by = c('Year', 'Day')] %>%
  ggplot(aes(Day, TripCount)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  facet_wrap(~ as.factor(Year)) +
  labs(x = '', y = 'Trip Count', title = 'Trip count by day of week')

g25

# Trip distance
g26 <- taxi_df[, .(TripDistance = `Trip Miles`,
                   Day = factor(weekdays(as.Date(date_time)), levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))), by = Year] %>%
  ggplot(aes(Day, TripDistance)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  facet_wrap(~ as.factor(Year)) +
  labs(x = '', y = 'Trip Distance', title = 'Trip distance by day of week')

g26

## By Hour of the day
# Trip Duration
g27 <- taxi_df[, .(TripDuration = `Trip Seconds`, Hour = strftime(date_time, '%H')), by = Year] %>%
  ggplot(aes(Hour, TripDuration)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  facet_wrap(~ as.factor(Year)) +
  labs(x = 'Hour', y = 'Trip Duration (seconds)', title = 'Trip duration across hour of day')

g27

# Trip Count
g28 <- taxi_df[, .(Hour = strftime(date_time, '%H')), by = Year][, .(TripCount = .N), by = c('Year', 'Hour')] %>%
  ggplot(aes(Hour, TripCount)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  facet_wrap(~ as.factor(Year)) +
  labs(x = 'Hour', y = 'Trip Count', title = 'Trip count across hour of day')

g28

# Distance covered
g29 <- taxi_df[, .(TripDistance = `Trip Miles`, Hour = strftime(date_time, '%H')), by = Year] %>%
  ggplot(aes(Hour, TripDistance)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  facet_wrap(~ as.factor(Year)) +
  labs(x = 'Hour', y = 'Trip Count', title = 'Trip distance by hour of day')

g29

## Correlation between predictors
c1 <- taxi_df[, .(TripPickUpMonth = as.numeric(strftime(date_time, '%m')),
                  TripPickUpDay = as.numeric(strftime(date_time, '%d')),
                  TripPickUpHour = as.numeric(strftime(date_time, '%H')),
                  TripDropOffMonth = as.numeric(strftime(date_time2, '%m')),
                  TripDropOffDay = as.numeric(strftime(date_time2, '%d')),
                  TripDropOffHour = as.numeric(strftime(date_time2, '%H')),
                  TripDuration = `Trip Seconds`,
                  TripDistance = `Trip Miles`,
                  TripTotal = as.numeric(gsub('[$]', '', `Trip Total`)))]
corrplot(cor(c1, use = 'complete.obs'), type = 'lower')
