## Set Working Directory
setwd("C:/Users/Jose/Documents/Comp_Data/Taxi_Data/ARIMA Data")
library(data.table)
## Import Data (From Subset 1)
Count_Data <- read.csv("ARIMACount.csv")
Fare_Data <- read.csv("ARIMAFare.csv")
Total_Data <- read.csv("ARIMATotal.csv")
Full_Data <- fread("FullTaxi.csv")
## Change Data type to Date

Count_Data <- Count_Data[-1465,] 
Count_Data$Date <- as.Date(Count_Data$Date)
Fare_Data$Date <- as.Date(Fare_Data$Date)
Total_Data$Date <- as.Date(Total_Data$Date)




########## Time Series: Count Data ##########
ggplot(Count_Data, aes(Date, Count)) + geom_line() + scale_x_date("Year")  + ylab("Taxi Rides") +
  xlab("") + ggtitle("Taxi Count") + theme(plot.title = element_text(hjust = 0.5))

count_ts <- ts(Count_Data[,c('Count')])
Count_Data$clean_count <- tsclean(count_ts)

ggplot() +
  geom_line(data = Count_Data, aes(x = Date, y = clean_count)) +  scale_x_date('Year') + ylab('Taxi Rides')+ ggtitle("Taxi Count (Cleaned)") + theme(plot.title = element_text(hjust = 0.5))

## Calculates Moving averages for Month and Week
Count_Data$count_ma = ma(Count_Data$clean_count, order = 7) # using the clean count with no outliers
Count_Data$count_ma30 = ma(Count_Data$clean_count, order = 30)

## Plots Counts with Moving AVerage for Week and Month
ggplot() +
  geom_line(data = Count_Data, aes(x = Date, y = clean_count, colour = "Counts")) +
  geom_line(data = Count_Data, aes(x = Date, y = count_ma,   colour = "Weekly Moving Average"))  +
  geom_line(data = Count_Data, aes(x = Date, y = count_ma30, colour = "Monthly Moving Average"))  +
  ylab('Taxi Rides')+ ggtitle("Taxi Count Moving Average") + theme(plot.title = element_text(hjust = 0.5))

## Calculates Seasonal components using stl
count_ma <- ts(na.omit(Count_Data$count_ma), frequency=30)
decomp1 <- stl(count_ma, s.window="periodic")

## seasadj() removes the seasonality by subtracting the seasonal component from the original series
deseasonal_count <- seasadj(decomp1)
plot(decomp1, main = "Taxi Count Trends")

Arima_Count <- Arima(ts(deseasonal_count, frequency = 365), order = c(1,1,1), seasonal = c(0,1,0))
Count_Forecast <- forecast(Arima_Count, h = 365)
plot(Count_Forecast)

# 
# ## ADF tests for stationarity. Ho: Series is non-stationary
# adf.test(count_ma, alternative = "stationary")  ## Non-stationary because of large p-val
# 
# ## Autocorrelation Plot
# Acf(count_ma, main='')
# ## PArtial Autocorrelation Plot
# Pacf(count_ma, main='')
# 
# ## Differs by 1
# count_d1 = diff(deseasonal_count, differences = 1)
# plot(count_d1)
# 
# adf.test(count_d1, alternative = "stationary") 
# 
# ## ACF and PACF for Differenced Series
# Acf(count_d1, main='ACF for Differenced Series')  #Sig at 1, 2 and beyond
# Pacf(count_d1, main='PACF for Differenced Series')
#
# tsdisplay(residuals(fit), lag.max=45, main='(3,1,4) Model Residuals')
# 
# fit2 = arima(deseasonal_count, order=c(3,1,7))
# fit2
# 
# tsdisplay(residuals(fit2), lag.max=15, main="Seasonal Model Residuals")
# 
# fcast <- forecast(fit2, h=30)
# plot(fcast, xlab = "Months", ylab = "Taxi Count")
# 
# ## "Starts" data earlier on
# hold <- window(ts(deseasonal_count), start=1400)
# ## Segments part of data to forecast
# fit_no_holdout = arima(ts(deseasonal_count[-c(1400:1455)]), order=c(3,1,7))
# ## Forecasts
# fcast_no_holdout <- forecast(fit_no_holdout,h=55)
# ## Plots
# plot(fcast_no_holdout, main=" ")
# lines(ts(deseasonal_count))
# 
# fit_w_seasonality = auto.arima(deseasonal_count, seasonal=TRUE)
# fit_w_seasonality
# 
# seas_fcast <- forecast(fit_w_seasonality, h=50)
# plot(seas_fcast)


########## Time Series: Median Fare ##########
ggplot(Fare_Data, aes(Date, Median)) + geom_line() + scale_x_date('Day')  + ylab("Median Taxi Fairs (Daily)") +
  xlab("") + ggtitle("Median Base Fare") + theme(plot.title = element_text(hjust = 0.5))

med_ts <- ts(Fare_Data[,c('Median')])
Fare_Data$clean_med <- tsclean(med_ts)

ggplot() +
  geom_line(data = Fare_Data, aes(x = Date, y = clean_med)) + ylab('Cleaned Taxi Median Fair')

## Calculates Moving averages for Month and Week
Fare_Data$med_ma = ma(Fare_Data$clean_med, order = 7) # using the clean count with no outliers
Fare_Data$med_ma30 = ma(Fare_Data$clean_med, order = 30)

## Plots Counts with Moving AVerage for Week and Month
ggplot() +
  geom_line(data = Fare_Data, aes(x = Date, y = clean_med, colour = "Median Taxi Fare")) +
  geom_line(data = Fare_Data, aes(x = Date, y = med_ma,   colour = "Weekly Moving Average"))  +
  geom_line(data = Fare_Data, aes(x = Date, y = med_ma30, colour = "Monthly Moving Average"))  +
  ylab('Taxi Fare')

## Calculates Seasonal components using stl
med_ma <- ts(na.omit(Fare_Data$med_ma), frequency=30)
decomp2 <- stl(med_ma, s.window="periodic")

## seasadj() removes the seasonality by subtracting the seasonal component from the original series
deseasonal_med <- seasadj(decomp2)
plot(decomp2)

Arima_med <- Arima(ts(deseasonal_med, frequency = 365), order = c(1,1,2), seasonal = c(0,1,0))
Med_Forecast <- forecast(Arima_med, h = 365)
plot(Med_Forecast)
# ## ADF tests for stationarity. Ho: Series is non-stationary
# adf.test(med_ma, alternative = "stationary")  ## Non-stationary because of large p-val
# 
# ## Autocorrelation Plot
# Acf(med_ma, main='')
# ## PArtial Autocorrelation Plot
# Pacf(med_ma, main='')
# 
# ## Differs by 1
# med_d1 = diff(deseasonal_med, differences = 1)
# plot(med_d1)
# 
# adf.test(med_d1, alternative = "stationary") 
# 
# ## ACF and PACF for Differenced Series
# Acf(med_d1, main='ACF for Differenced Series')  #Sig at 1, 2 and beyond
# Pacf(med_d1, main='PACF for Differenced Series')
# 
# auto.arima(deseasonal_med, seasonal=FALSE)
# fit <- auto.arima(deseasonal_med, seasonal=FALSE)
# 
# tsdisplay(residuals(fit), lag.max=45, main='(1,1,3) Model Residuals')
# 
# fit2 = arima(deseasonal_med, order=c(1,1,3))
# fit2
# 
# tsdisplay(residuals(fit2), lag.max=15, main="Seasonal Model Residuals")
# 
# fcast <- forecast(fit2, h=30)
# plot(fcast, xlab = "Months", ylab = "Taxi Count")
# 
# # 
# # hold <- window(ts(deseasonal_cnt), start=1400)
# # fit_no_holdout = arima(ts(deseasonal_cnt[-c(1400:1455)]), order=c(1,1,3))
# # fcast_no_holdout <- forecast(fit_no_holdout,h=55)
# # plot(fcast_no_holdout, main=" ")
# # lines(ts(deseasonal_cnt))
# # 
# # fit_w_seasonality_med = auto.arima(deseasonal_med, seasonal=TRUE)
# # fit_w_seasonality_med
# # 
# # seas_fcast_med <- forecast(fit_w_seasonality_med, h=50)
# # plot(seas_fcast_med)
# 
# 
# # 
# # fit_w_seasonality = auto.arima(deseasonal_cnt, seasonal=TRUE, D = 1)
# # fit_w_seasonality
# 
# start_time <- Sys.time()
# plot(forecast(auto.arima(ts(deseasonal_med, frequency = 365), D = 1), h=365), main = "Taxi Median Fare", ylim = c(0,max(taxi_med$Median)))
# plot(forecast(auto.arima(ts(deseasonal_med, frequency = 365), D = 1, parallel = TRUE, stepwise = FALSE, seasonal = TRUE), h=365), main = "Taxi Median Fare", ylim = c(0,max(taxi_med$Median)))
# 
# end_time <- Sys.time()
# tot_time<- end_time - start_time



########## Time Series: Median Total Cost ##########
ggplot(Total_Data, aes(Date, Median)) + geom_line() + scale_x_date('Day')  + ylab("Median Taxi Total Cost (Daily)") +
  xlab("") + ggtitle("Median Total Cost") + theme(plot.title = element_text(hjust = 0.5))

med_total_ts <- ts(Total_Data[,c('Median')])
Total_Data$clean_med_total <- tsclean(med_total_ts)

ggplot() +
  geom_line(data = Total_Data, aes(x = Date, y = clean_med_total)) + ylab('Cleaned Taxi Median Total Cost')

## Calculates Moving averages for Month and Week
Total_Data$med_total_ma = ma(Total_Data$clean_med_total, order = 7) # using the clean count with no outliers
Total_Data$med_total_ma30 = ma(Total_Data$clean_med_total, order = 30)

## Plots Counts with Moving AVerage for Week and Month
ggplot() +
  geom_line(data = Total_Data, aes(x = Date, y = clean_med_total, colour = "Median Taxi Total Cost")) +
  geom_line(data = Total_Data, aes(x = Date, y = med_total_ma,   colour = "Weekly Moving Average"))  +
  geom_line(data = Total_Data, aes(x = Date, y = med_total_ma30, colour = "Monthly Moving Average"))  +
  ylab('Total Cost (In Dollars)')

## Calculates Seasonal components using stl
med_total_ma <- ts(na.omit(Total_Data$med_total_ma), frequency=30)
decomp3 <- stl(med_total_ma, s.window="periodic")


## seasadj() removes the seasonality by subtracting the seasonal component from the original series
deseasonal_med_total <- seasadj(decomp3)
plot(decomp3)

Arima_Total <- Arima(ts(deseasonal_med_total, frequency = 365),order = c(5,1,2), seasonal = c(0,1,0))
Total_Forecast <- forecast(Arima_Total, h = 365)
plot(Total_Forecast)

Arima_Total_Adjusted <- Arima(ts(deseasonal_med_total, frequency = 365),order = c(1,1,2), seasonal = c(0,1,0))
Total_Adjusted_Forecast<- forecast(Arima_Total_Adjusted, h = 365)
plot(Total_Adjusted_Forecast)
