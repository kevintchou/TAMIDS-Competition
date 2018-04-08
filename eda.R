setwd('~/Documents/TAMIDS Competition')

library(data.table)
library(dplyr)
library(magrittr)
library(MASS)
library(ggplot2)
library(gridExtra)
library(grep)

## Notes
# taxi 2013 has ~ 33.5 million rows
# taxi 2014 has ~ 31 million rows
# taxi 2015 has ~ 27.4 million rows
# taxi 2016 has ~ 19.9 million rows
# taxi 2016 has ~ 7.68 million rows
#
# Currently using a 2% random subset of the full data

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


