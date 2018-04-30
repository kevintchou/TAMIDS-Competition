setwd("C:/Users/Jose/Documents/Comp_Data/Taxi_Data/ARIMA Data")
library(data.table)
library(dplyr)
library(magrittr)
library(MASS)
library(ggplot2)
library(gridExtra)
library(randomForest)
library(mlr)
library(forecast)
library(tseries)

#taxi_2013 <- fread('Chicago_taxi_trips2013.csv')[,c(2,3,13)]
#taxi_2014 <- fread('Chicago_taxi_trips2014.csv')[,c(2,3,12)]
#taxi_2015 <- fread('Chicago_taxi_trips2015.csv')[,c(2,3,12)]
#taxi_2016 <- fread('Chicago_taxi_trips2016.csv')[,c(2,3,12)]
taxi_2017 <- fread('Chicago_taxi_trips2017.csv')[,c(2,3,12)]
taxi_2017COPY <- taxi_2017
#names(taxi_2014) <- names(taxi_2013)
#names(taxi_2015) <- names(taxi_2013)
#names(taxi_2016) <- names(taxi_2013)

#names(taxi_2017) <- names(taxi_df[,1:3])
#taxi_df <- data.table(rbindlist(list(taxi_2013, taxi_2014, taxi_2015, taxi_2016)))
#taxi_df <- na.omit(taxi_df)
# taxi_2017COPY$Fare <- ifelse(taxi_2017COPY$Fare, NA, taxi_2017COPY$Fare)
taxi_2017COPY$Fare[taxi_2017COPY$Fare == ""] <- NA
taxi_2017COPY <- na.omit(taxi_2017COPY)

#date_time <- strptime(taxi_df$`Trip Start Timestamp`, '%m/%d/%Y %I:%M:%S %p')
date_time_2017 <- strptime(taxi_2017COPY$`Trip Start Timestamp`, '%m/%d/%Y %I:%M:%S %p')

# taxi_2017[, `:=` (Month = as.numeric(strftime(date_time_2017, '%m')),
#                   Year = as.numeric(strftime(date_time_2017, '%Y')),
#                   Day = as.numeric(strftime(date_time_2017, "%d")),
#                   Date = as.Date(strftime(date_time_2017, "%Y-%m-%d")),
#                   ]
taxi_2017COPY$Month <- as.numeric(strftime(date_time_2017, '%m'))
taxi_2017COPY$Year <- as.numeric(strftime(date_time_2017, '%Y'))
taxi_2017COPY$Day <- as.numeric(strftime(date_time_2017, "%d"))
taxi_2017COPY$Date <- strftime(date_time_2017, "%Y-%m-%d")

#taxi_2017$X <- 1:nrow(taxi_2017)

taxi_med17 <- taxi_2017COPY[, .(TotalFare = sum(as.numeric(gsub('[$]', '', Fare))), Date), by = list(`Taxi ID`, Month, Day, Year)][, .(Median = median(TotalFare)), by = Date][order(Date), ]

# taxi_med_2017 <- taxi_2017[, .(TotalFare = sum(as.numeric(gsub('[$]', '', Fare))),
#                         Date), by = list(`Taxi ID`, Month, Day, Year)][, .(Median = median(TotalFare)), by = Date][order(Date), ]
# $Date <- as.Date(with(taxi_med, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")
# Fare_Data <- taxi_med
taxi_med_2017 <- taxi_2017[, .(TotalFare = sum(as.numeric(gsub('[$]', '', Fare)))), by = list(`Taxi ID`, Month, Day, Year)][, .(Median = median(TotalFare)), by = list(Month, Day, Year)][order(Year, Month, Day), ]
taxi_med$Date <- as.Date(with(taxi_med, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")


#med_ts <- ts(Fare_Data[,c('Median')])
#Fare_Data$clean_med <- tsclean(med_ts)

ggplot() +
  geom_line(data = Fare_Data, aes(x = Date, y = clean_med)) + ylab('Cleaned Taxi Median Fare') +
  ggtitle("Median Daily Fare") + theme(plot.title = element_text(hjust = .5))

## Calculates Moving averages for Month and Week
#Fare_Data$med_ma = ma(Fare_Data$clean_med, order = 7) # using the clean count with no outliers
#Fare_Data$med_ma30 = ma(Fare_Data$clean_med, order = 30)
## Plots Counts with Moving AVerage for Week and Month
ggplot() +
  geom_line(data = Fare_Data, aes(x = Date, y = clean_med, colour = "Median Taxi Fare")) +
  geom_line(data = Fare_Data, aes(x = Date, y = med_ma,   colour = "Weekly Moving Average"))  +
  geom_line(data = Fare_Data, aes(x = Date, y = med_ma30, colour = "Monthly Moving Average"))  +
  ylab('Taxi Fare')  +
  ggtitle("Median Daily Fare") + theme(plot.title = element_text(hjust = .5))

## Calculates Seasonal components using stl
#med_ma <- ts(na.omit(Fare_Data$med_ma), frequency=52)
#decomp <- stl(med_ma, s.window="periodic")

## seasadj() removes the seasonality by subtracting the seasonal component from the original series
deseasonal_med <- seasadj(decomp)
plot(decomp, main = 'Median Fare Decomposition')
plot(deseasonal_med, main = 'Deseasonal Moving Average')

adf.test(med_ma, alternative = "stationary")
adf.test(deseasonal_med, alternative = "stationary")
Acf(med_ma, main='Median Moving Average') ## EXAMPLE PURPOSES ONLY NOT ANALYSIS
Pacf(med_ma, main='Median Moving Average') ## SEE ABOVE
med_d1 <- diff(deseasonal_med,difference = 1)
plot(med_ma)
plot(med_d1, main = 'Differentiated Moving Average', ylab = '')
adf.test(med_d1, alternative = 'stationary')
Acf(med_d1, main='Differenced Median Moving Average')
Pacf(med_d1, main='Differenced Median Moving Average')

#Arima_med_naive <- Arima(ts(deseasonal_med, frequency = 365), order = c(3,1,8))
#Med_Forecast_naive <- forecast(Arima_med_naive, h = 365)
plot(Med_Forecast_naive, main = 'Naive Model: ARIMA(3,1,8)', ylab = 'Median Daily Fare in $', xaxt = 'n')
axis(1, at=1:6, labels=2013:2018)

#Arima_med_naive_seas <- Arima(ts(deseasonal_med, frequency = 365), order = c(3,1,8), seasonal = c(0,1,0))
#Med_Forecast_naive_seas <- forecast(Arima_med_naive_seas, h = 365)
plot(Med_Forecast_naive_seas, main = 'Naive Seasonal Model: ARIMA(3,1,8)', ylab = 'Median Daily Fare in $', xaxt = 'n')


#autoarima <- auto.arima(deseasonal_med, D = 1, approximation = TRUE)
#equivalent to autoarima <- Arima(ts(deseasonal_med,frequency = 365), order = c(4,1,4), seasonal = c(0,1,0))
plot(forecast(autoarima, h = 365), main = 'Stepwise AIC-Minimized Seasonal Model: ARIMA(4,1,4)', ylab = 'Median Daily Fare in $', xaxt = 'n')
taxi_med17$Fitted <- tail(Med_Forecast_naive_seas$fitted, n = 212)
taxi_med17$AAFitted <- tail(autoarima$fitted, n = 212)
ggplot(taxi_med17) + aes(x = Date, y = Median) + geom_point() + geom_smooth(method = "lm")
plot_internal

ggplot(taxi_med17) + aes(x = Date) + geom_point(aes(y = Median)) + geom_point(aes(y = Fitted), color = "purple") + geom_point(aes(y = AAFitted), color = "red") + geom_segment(aes(xend = Date, y = Fitted, yend = Median), arrow = arrow(length = unit(0.4, "line")), color = "blue")



#tsfit <- tslm(Median~trend + season, data = ts(Fare_Data, freq = 365))
tsfit
