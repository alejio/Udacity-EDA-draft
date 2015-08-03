
# Load auxilliary data in order to merge with main ---------


# Establishment data set --------------------------------------------------


#Load data
# I found this data set on https://www.census.gov/econ/geo-zip.html (i think)
# or here http://factfinder.census.gov/
# faces/affhelp/jsf/pages/metadata.xhtml?lang=en&type=dataset&id=dataset.en.BP_2011
est_data <- fread('./data/raw/iowa_food_drink.csv', header = T, sep = ',', verbose=TRUE)

#Pre-process data

library(tidyr)
library(dplyr)
#Drop unnecessary columns. Converted to data frame.
est_data <- select(as.data.frame(est_data), -GEOGRAPHY, -NAICS2007)
#Remove duplicates
est_data <- est_data[!duplicated(est_data2),]
#Transform dataset
est_data <- spread(est_data, NAICS2007_MEANING, ESTAB)
#Set missing to zero
est_data[is.na(est_data)] <- 0
#Make it a data table again
est_data <- as.data.table(est_data)


# Population data set -----------------------------------------------------


# Load data
# I don't remember where I got this data from
pop_data <- fread('./data/raw/Iowa_population.csv', header = T, sep = ',', verbose=TRUE)


# Weather data set --------------------------------------------------------

# Ordered daily data for 2014 from
#http://www.ncdc.noaa.gov/cdo-web/datatools/records
# Unfortunately data was very incomplete.

## Load data
weather_data <- fread('./data/raw/Iowa_weather.csv', header = T, 
                      sep = ',', verbose=TRUE)

## Pre-condition data
weather_data <- select(weather_data, -STATION, -TSUN, -AWND, -WT09, -WT01,
                       -WT06, -WT05, -WT02, -WT11, -WT04, -WT08, -WT03)
weather_data$ELEVATION <- as.numeric(weather_data$ELEVATION)
weather_data$LATITUDE <- as.numeric(weather_data$LATITUDE)
weather_data$LONGITUDE <- as.numeric(weather_data$LONGITUDE)
weather_data$DATE <- as.Date(as.character(weather_data$DATE), "%Y%m%d")
#Celsius convert. range of T is -200 to 200; something wrong
#weather_data$TMIN <- (weather_data$TMIN-32)*5/9

# Measurements do not include zipcode, so we need to associate zipcode to
#latitude and longitude. For this purpose, I pulled the relevant data from
#http://www.geonames.org/export/web-services.html#findNearbyPostalCodes
#by using their really nice API in Python (query input lat/lon and output
#is zipcode. I then made a .csv)

# Insert associated zipcode - lat/lon data
zip_lat_lon_data <- fread('./data/processed/ZIPCODES_LATLON.csv',
                          header = T, sep = ',', verbose=TRUE)
# Merge data sets
weather_data <- left_join(weather_data, zip_lat_lon_data,
                          by=c("LATITUDE", "LONGITUDE"))
weather_data <- select(weather_data, -V1)
#NAs in ZIPCODE. Possibly latitude and longitude not exactly same.
#Trying to populate based on min distances.
#Get NA subset to work with
nas.lon <- weather_data$LONGITUDE[is.na(weather_data$ZIPCODE)]
nas.lat <- weather_data$LATITUDE[is.na(weather_data$ZIPCODE)]
nas <- data.frame(nas.lon, nas.lat)
nas <- nas[!duplicated(nas),]
nas <- na.omit(nas)
ref.LLZ <- as.data.frame(select(zip_lat_lon_data, LONGITUDE,
                                LATITUDE, ZIPCODE))
library("geosphere")
# Code to add zipcode in intermediate a3 data.frame that had NA's.
# To be merged to main data1.
for (i.na in seq(1,nrow(nas))) {
  b <- c()
  for (j.ref in seq(1,nrow(ref.LLZ))) {
    a <- distGeo(nas[i.na,], ref.LLZ[j.ref, 1:2])
    b <- append(a, b)
  }
  nas[i.na,3] <- ref.LLZ[which.min(b),3]
}
colnames(nas)[1:3] <- c("LONGITUDE", "LATITUDE", "ZIPCODE")

# Apply missing values fix
# Missing values condition
cond <- (is.na(weather_data$ZIPCODE))&(!is.na(weather_data$LATITUDE))
# Split data set into "good" and "bad"
weather_data.nas <- select(weather_data[cond], -ZIPCODE)
weather_data.nas <- left_join(weather_data.nas, nas, 
                              by=c("LATITUDE", "LONGITUDE"),copy=TRUE)
weather_data.good <- weather_data[!cond]
# Rejoin them after fixing "bad" subset
weather_data <- rbind(weather_data.nas, weather_data.good)
# Do some renaming so we don't have any conlicts with main during merge
setnames(weather_data, "LONGITUDE", "W_LONGITUDE")
setnames(weather_data, "LATITUDE", "W_LATITUDE")
setnames(weather_data, "STATION_NAME", "W_STATION_NAME")
# Decided to keep only these columns; rain, snow, min/max temp
weather_data <- select(weather_data,
                       c(DATE, ZIPCODE, PRCP, SNOW, TMIN, TMAX))
#Convert temps as there is something wrong! (to Celsius)
#weather_data$TMIN <- (weather_data$TMIN/10 - 32)*0.5556
#weather_data$TMAX <- (weather_data$TMAX/10 - 32)*0.5556


data1 <- fread('./data/processed/IA_unis.csv',
               header = T, sep = ',', verbose=TRUE)


# Universities data set ---------------------------------------------------

unis <- fread('./data/processed/IA_unis.csv', header = T,
              sep = ',', verbose=TRUE)

#No need to merge with master; just for plotting