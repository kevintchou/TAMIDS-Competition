library(data.table)
setwd('/share/RDirectory')
Chi2017 <- fread('Chicago_taxi_trips2017.csv')

date1 <- substr(Chi2017$`Trip Start Timestamp`,1,10)
month1 <- substr(Chi2017$`Trip Start Timestamp`,1,2)
day1 <- substr(Chi2017$`Trip Start Timestamp`,4,5)
year1 <-substr(Chi2017$`Trip Start Timestamp`,7,10)

date2 <- substr(Chi2017$`Trip End Timestamp`,1,10)
month2 <- substr(Chi2017$`Trip End Timestamp`,1,2)
day2 <- substr(Chi2017$`Trip End Timestamp`,4,5)
year2 <-substr(Chi2017$`Trip End Timestamp`,7,10)

#Uneven time stamp lengths
time1 <- substr(Chi2017$`Trip Start Timestamp`,2,22)
AMPM1 <- substr(Chi2017$`Trip Start Timestamp`,20,21)
hour1 <- substr(Chi2017$`Trip Start Timestamp`,2,21)
time1 <- substr(Chi2017$`Trip Start Timestamp`,2,21)

