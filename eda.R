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

taxi_df <- data.table(rbindlist(list(taxi_2013, taxi_2014, taxi_2015, taxi_2016)),
                      Year = rep(c(2013, 2014, 2015, 2016), times = c(nrow(taxi_2013), nrow(taxi_2014), nrow(taxi_2015), nrow(taxi_2016))))

## Earnings by company
# 2013
g1 <- taxi_2013[, .(TripTotal = sum(as.numeric(gsub('[$]', '', V17)), na.rm = TRUE)), by = V19][1:10,] %>% 
  ggplot(aes(reorder(V19, TripTotal), TripTotal)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  labs(x = '', y = 'Trip Total', title = '2013') +
  coord_flip()

# 2014
g2 <- taxi_2014[, .(TripTotal = sum(as.numeric(gsub('[$]', '', V16)), na.rm = TRUE)), by = V18][1:10,] %>% 
  ggplot(aes(reorder(V18, TripTotal), TripTotal)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  labs(x = '', y = 'Trip Total', title = '2014') +
  coord_flip()

# 2015
g3 <- taxi_2015[, .(TripTotal = sum(as.numeric(gsub('[$]', '', V16)), na.rm = TRUE)), by = V18][1:10,] %>% 
  ggplot(aes(reorder(V18, TripTotal), TripTotal)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  labs(x = '', y = 'Trip Total', title = '2015') +
  coord_flip()

# 2016
g4 <- taxi_2016[, .(TripTotal = sum(as.numeric(gsub('[$]', '', V16)), na.rm = TRUE)), by = V18][1:10,] %>% 
  ggplot(aes(reorder(V18, TripTotal), TripTotal)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  labs(x = '', y = 'Trip Total', title = '2016') +
  coord_flip()

# 2017
g5 <- taxi_2017[, .(TripTotal = sum(as.numeric(gsub('[$]', '', V16)), na.rm = TRUE)), by = V18][1:10,] %>% 
  ggplot(aes(reorder(V18, TripTotal), TripTotal)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  labs(x = '', y = 'Trip Total', title = '2017') +
  coord_flip()

grid.arrange(g1, g2, g3, g4, g5, nrow = 2)

## Average Fare per Trip for each company
# 2013
g6 <- taxi_2013[, .(AvgFare = mean(as.numeric(gsub('[$]', '', V13)), na.rm = TRUE)), by = V19][1:10,] %>%
  ggplot(aes(reorder(V19, AvgFare), AvgFare)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  labs(x = '', y = 'Average fare per trip', title = '2013') +
  coord_flip()

# 2014
g7 <- taxi_2014[, .(AvgFare = mean(as.numeric(gsub('[$]', '', V12)), na.rm = TRUE)), by = V18][1:10,] %>%
  ggplot(aes(reorder(V18, AvgFare), AvgFare)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  labs(x = '', y = 'Average fare per trip', title = '2014') +
  coord_flip()

# 2015
g8 <- taxi_2015[, .(AvgFare = mean(as.numeric(gsub('[$]', '', V12)), na.rm = TRUE)), by = V18][1:10,] %>%
  ggplot(aes(reorder(V18, AvgFare), AvgFare)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  labs(x = '', y = 'Average fare per trip', title = '2015') +
  coord_flip()

# 2016
g9 <- taxi_2016[, .(AvgFare = mean(as.numeric(gsub('[$]', '', V12)), na.rm = TRUE)), by = V18][1:10,] %>%
  ggplot(aes(reorder(V18, AvgFare), AvgFare)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  labs(x = '', y = 'Average fare per trip', title = '2016') +
  coord_flip()

# 2017
g10 <- taxi_2017[, .(AvgFare = mean(as.numeric(gsub('[$]', '', V12)), na.rm = TRUE)), by = V18][1:10,] %>%
  ggplot(aes(reorder(V18, AvgFare), AvgFare)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  labs(x = '', y = 'Average fare per trip', title = '2017') +
  coord_flip()

grid.arrange(g6, g7, g8, g9, g10, nrow = 2)

## Average Distance covered by each company
# 2013
g11 <- taxi_2013[, .(TripMiles = mean(V6, na.rm = TRUE)), by = V19][1:10,] %>%
  ggplot(aes(reorder(V19, TripMiles), TripMiles)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  labs(x = '', y = 'Average Miles per trip', title = '2013') +
  coord_flip()

# 2014
g12 <- taxi_2014[, .(TripMiles = mean(V6, na.rm = TRUE)), by = V18][1:10,] %>%
  ggplot(aes(reorder(V18, TripMiles), TripMiles)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  labs(x = '', y = 'Average Miles per trip', title = '2014') +
  coord_flip()

# 2015
g13 <- taxi_2015[, .(TripMiles = mean(V6, na.rm = TRUE)), by = V18][1:10,] %>%
  ggplot(aes(reorder(V18, TripMiles), TripMiles)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  labs(x = '', y = 'Average Miles per trip', title = '2015') +
  coord_flip()

# 2016
g14 <- taxi_2016[, .(TripMiles = mean(V6, na.rm = TRUE)), by = V18][1:10,] %>%
  ggplot(aes(reorder(V18, TripMiles), TripMiles)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  labs(x = '', y = 'Average Miles per trip', title = '2016') +
  coord_flip()

# 2017
g15 <- taxi_2017[, .(TripMiles = mean(V6, na.rm = TRUE)), by = V18][1:10,] %>%
  ggplot(aes(reorder(V18, TripMiles), TripMiles)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  labs(x = '', y = 'Average Miles per trip', title = '2017') +
  coord_flip()

grid.arrange(g11, g12, g13, g14, g15, nrow = 2)

## Distance covered v. amount of tips 
# Takes a while for the plots to load
g16 <- data.table(Year = rep(c(2013, 2014, 2015, 2016, 2017), times = c(nrow(taxi_2013), nrow(taxi_2014), nrow(taxi_2015), nrow(taxi_2016), nrow(taxi_2017))),
                  Distance = c(taxi_2013$V6, taxi_2014$V6, taxi_2015$V6, taxi_2016$V6, taxi_2017$V6),
                  Tip = c(as.numeric(gsub('[$]', '', taxi_2013$V14)), as.numeric(gsub('[$]', '', taxi_2014$V13)), as.numeric(gsub('[$]', '', taxi_2015$V13)), as.numeric(gsub('[$]', '', taxi_2016$V13)), as.numeric(gsub('[$]', '', taxi_2017$V13)))) %>% 
  ggplot(aes(x = Distance, y = Tip)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap( ~ as.factor(Year)) +
  labs(x = 'Distance (miles)', y = '$ Tips', title = '$ Tips Received v. Distance Covered')

g16

## Trip Duration Analysis
# Density of trip duration
g17 <- taxi_2013[, .(TripDuration = V5)] %>%
  ggplot(aes(x = TripDuration)) +
  geom_histogram(stat = 'bin', fill = 'steelblue') +
  ylab('Density') +
  labs(x = 'Trip Duration (seconds)', title = '2013') +
  xlim(0, 4000)

g18 <- taxi_2014[, .(TripDuration = V5)] %>%
  ggplot(aes(x = TripDuration)) +
  geom_histogram(stat = 'bin', fill = 'steelblue') +
  ylab('Density') +
  labs(x = 'Trip Duration (seconds)', title = '2014') +
  xlim(0, 4000)

g19 <- taxi_2015[, .(TripDuration = V5)] %>%
  ggplot(aes(x = TripDuration)) +
  geom_histogram(stat = 'bin', fill = 'steelblue') +
  ylab('Density') +
  labs(x = 'Trip Duration (seconds)', title = '2015') +
  xlim(0, 4000)

g20 <- taxi_2016[, .(TripDuration = V5)] %>%
  ggplot(aes(x = TripDuration)) +
  geom_histogram(stat = 'bin', fill = 'steelblue') +
  ylab('Density') +
  labs(x = 'Trip Duration (seconds)', title = '2016') +
  xlim(0, 4000)

g21 <- taxi_2017[, .(TripDuration = V5)] %>%
  ggplot(aes(x = TripDuration)) +
  geom_histogram(stat = 'bin', fill = 'steelblue') +
  ylab('Density') +
  labs(x = 'Trip Duration (seconds)', title = '2017') +
  xlim(0, 4000)
grid.arrange(g17, g18, g19, g20, g21, nrow = 2)

# Trip duration over time
date_time_2013 <- strptime(taxi_2013$V3, format = '%m/%d/%Y %H:%M:%S %p')
date_time_2014 <- strptime(taxi_2014$V3, format = '%m/%d/%Y %H:%M:%S %p')
date_time_2015 <- strptime(taxi_2015$V3, format = '%m/%d/%Y %H:%M:%S %p')
date_time_2016 <- strptime(taxi_2016$V3, format = '%m/%d/%Y %H:%M:%S %p')
date_time_2017 <- strptime(taxi_2017$V3, format = '%m/%d/%Y %H:%M:%S %p')

# Month
g22 <- taxi_2013[, .(TripDuration = V5, Month = strftime(date_time_2013, '%m'))] %>%
  ggplot(aes(x = Month, y = TripDuration)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  labs(x = '', y = 'Trip Duration (seconds)', title = '2013')

g23 <- taxi_2014[, .(TripDuration = V5, Month = strftime(date_time_2014, '%m'))] %>%
  ggplot(aes(x = Month, y = TripDuration)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  labs(x = '', y = 'Trip Duration (seconds)', title = '2014')

g24 <- taxi_2015[, .(TripDuration = V5, Month = strftime(date_time_2015, '%m'))] %>%
  ggplot(aes(x = Month, y = TripDuration)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  labs(x = '', y = 'Trip Duration (seconds)', title = '2015')

g25 <- taxi_2016[, .(TripDuration = V5, Month = strftime(date_time_2016, '%m'))] %>%
  ggplot(aes(x = Month, y = TripDuration)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  labs(x = '', y = 'Trip Duration (seconds)', title = '2016')

g26 <- taxi_2017[, .(TripDuration = V5, Month = strftime(date_time_2017, '%m'))] %>%
  ggplot(aes(x = Month, y = TripDuration)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  labs(x = '', y = 'Trip Duration (seconds)', title = '2017')
grid.arrange(g22, g23, g24, g25, g26, nrow = 2)

# Hour -- need to split by am/pm also
g27 <- taxi_2013[, .(TripDuration = V5, Month = strftime(date_time_2013, '%H'))] %>%
  ggplot(aes(x = Month, y = TripDuration)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  labs(x = '', y = 'Trip Duration (seconds)', title = '2013')

g28 <- taxi_2014[, .(TripDuration = V5, Month = strftime(date_time_2014, '%H'))] %>%
  ggplot(aes(x = Month, y = TripDuration)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  labs(x = '', y = 'Trip Duration (seconds)', title = '2014')

g29 <- taxi_2015[, .(TripDuration = V5, Month = strftime(date_time_2015, '%H'))] %>%
  ggplot(aes(x = Month, y = TripDuration)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  labs(x = '', y = 'Trip Duration (seconds)', title = '2015')

g30 <- taxi_2016[, .(TripDuration = V5, Month = strftime(date_time_2016, '%H'))] %>%
  ggplot(aes(x = Month, y = TripDuration)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  labs(x = '', y = 'Trip Duration (seconds)', title = '2016')

g31 <- taxi_2017[, .(TripDuration = V5, Month = strftime(date_time_2017, '%H'))] %>%
  ggplot(aes(x = Month, y = TripDuration)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  labs(x = '', y = 'Trip Duration (seconds)', title = '2017')
grid.arrange(g27, g28, g29, g30, g31, nrow = 2)
# Day of the week

# Number of trips -- month, day, hour
