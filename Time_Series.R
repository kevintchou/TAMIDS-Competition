## Set Work Directory
setwd("C:/Users/Jose/Documents/Taxi_Data")
## Import Subsetted Data
data_2013 <- read.csv("subset_2013.csv", header = F)
data_2014 <- read.csv("subset_2014.csv", header = F)
data_2015 <- read.csv("subset_2015.csv", header = F)
data_2016 <- read.csv("subset_2016.csv", header = F)
data_2017 <- read.csv("subset_2017.csv", header = F)

## Check Dimensions of Data
dim(data_2013)
dim(data_2014)
dim(data_2015)
dim(data_2016)
dim(data_2017)

## Add Column Names
# colnames(data_2013) <- c("Trip ID", "Taxi ID", "Trip Start Timestamp", "Trip End Timestamp", "Trip Seconds", "Trip Miles", "Pickup Census Tract", "Dropoff Census Tract", "Pickup Community Area", "Pickup O'Hare Community Area", "Dropoff Community Area","Fare", "Tips", "Tolls", "Extras", "Trip Total", "Payment Type", "Company", "Pickup Centroid Latitude", "Pickup Centroid Longitude", "Pickup Centroid Location","Dropoff Centroid Latitude", "Dropoff Centroid Longitude", "Dropoff Centroid Location")
colnames(data_2014) <- c("Trip ID", "Taxi ID", "Trip Start Timestamp", "Trip End Timestamp", "Trip Seconds", "Trip Miles", "Pickup Census Tract", "Dropoff Census Tract", "Pickup Community Area", "Pickup O'Hare Community Area", "Dropoff Community Area","Fare", "Tips", "Tolls", "Extras", "Trip Total", "Payment Type", "Company", "Pickup Centroid Latitude", "Pickup Centroid Longitude", "Pickup Centroid Location","Dropoff Centroid Latitude", "Dropoff Centroid Longitude", "Dropoff Centroid Location")
colnames(data_2015) <- c("Trip ID", "Taxi ID", "Trip Start Timestamp", "Trip End Timestamp", "Trip Seconds", "Trip Miles", "Pickup Census Tract", "Dropoff Census Tract", "Pickup Community Area", "Pickup O'Hare Community Area", "Dropoff Community Area","Fare", "Tips", "Tolls", "Extras", "Trip Total", "Payment Type", "Company", "Pickup Centroid Latitude", "Pickup Centroid Longitude", "Pickup Centroid Location","Dropoff Centroid Latitude", "Dropoff Centroid Longitude", "Dropoff Centroid Location")
colnames(data_2016) <- c("Trip ID", "Taxi ID", "Trip Start Timestamp", "Trip End Timestamp", "Trip Seconds", "Trip Miles", "Pickup Census Tract", "Dropoff Census Tract", "Pickup Community Area", "Pickup O'Hare Community Area", "Dropoff Community Area","Fare", "Tips", "Tolls", "Extras", "Trip Total", "Payment Type", "Company", "Pickup Centroid Latitude", "Pickup Centroid Longitude", "Pickup Centroid Location","Dropoff Centroid Latitude", "Dropoff Centroid Longitude", "Dropoff Centroid Location")
colnames(data_2017) <- c("Trip ID", "Taxi ID", "Trip Start Timestamp", "Trip End Timestamp", "Trip Seconds", "Trip Miles", "Pickup Census Tract", "Dropoff Census Tract", "Pickup Community Area", "Pickup O'Hare Community Area", "Dropoff Community Area","Fare", "Tips", "Tolls", "Extras", "Trip Total", "Payment Type", "Company", "Pickup Centroid Latitude", "Pickup Centroid Longitude", "Pickup Centroid Location","Dropoff Centroid Latitude", "Dropoff Centroid Longitude", "Dropoff Centroid Location")

## Number of NA's in Company Column
sum(is.na(data_2014$Company))
head(data_2014)
dim(data_2014)
x <- as.data.frame(data_2014)
x$Company
head(x, n=1)
