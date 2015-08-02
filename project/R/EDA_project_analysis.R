library(data.table)
data1 <- fread('./data/raw/Iowa_Liquor_Sales.csv', header = T, sep = ',', verbose=TRUE)
data1$DATE <- as.Date(data1$DATE, "%m/%d/%Y")
data1$ZIPCODE <- factor(data1$ZIPCODE, ordered=F)
data1$`STATE BTL COST` <- sapply(strsplit(data1$`STATE BTL COST`, split='$', fixed=TRUE), function(x) (x[2]))
data1$`STATE BTL COST` <- as.numeric(data1$`STATE BTL COST`)
data1$`BTL PRICE` <- sapply(strsplit(data1$`BTL PRICE`, split='$', fixed=TRUE), function(x) (x[2]))
data1$`BTL PRICE` <- as.numeric(data1$`BTL PRICE`)
data1$`TOTAL` <- sapply(strsplit(data1$`TOTAL`, split='$', fixed=TRUE), function(x) (x[2]))
data1$`TOTAL` <- as.numeric(data1$`TOTAL`)
data1 <- subset(data1, DATE<"2015-01-01")
##Insert establishment data

est_data <- fread('./data/raw/iowa_food_drink.csv', header = T, sep = ',', verbose=TRUE)

#Start merging
data_zipcodes <- unique(data1$ZIPCODE)

#Rogue zipcodes are 97 (sioux city, Morningside avenue) and NA (Dunlap). Dunlap is 61525 and Sioux city is 51106 (googled it)
summary(data_zipcodes)
min(na.omit(data_zipcodes))
max(na.omit(data_zipcodes))
sum(is.na(data1$ZIPCODE))
data1$CITY[is.na(data1$ZIPCODE)]
data1$ZIPCODE[is.na(data1$ZIPCODE)] <- 61525
data1$CITY[data1$ZIPCODE==97]
data1$ZIPCODE[data1$ZIPCODE==97] <- 51106

install.packages("tidyr")
library(tidyr)

library(dplyr)
#Drop columns
est_data2 <- select(as.data.frame(est_data), -GEOGRAPHY, -NAICS2007)
#Remove duplicates
est_data2 <- est_data2[!duplicated(est_data2),]
#Transform dataset
library(tidyr)
est_data3 <- spread(est_data2, NAICS2007_MEANING, ESTAB)
#Set missing to zero
names(data1)[23:45]
est_data3[is.na(est_data3)] <- 0
est_data3 <- as.data.table(est_data3)
#Join
data1 <- left_join(data1, est_data3, by = "ZIPCODE")
summary(data1)
#NA's present because zipcode was not in est_data3
data1 <- na.omit(data1)
summary(data1)
#Calculate total establishments
data1$TOTAL_EST <- rowSums(data1[seq(0,nrow(data1)), 23:45,with=FALSE])
summary(data1)

##Insert population data

pop_data <- fread('./data/raw/Iowa_population.csv', header = T, sep = ',', verbose=TRUE)
str(pop_data)

#Merge population data with base
data1 <- left_join(data1, pop_data, by = "ZIPCODE")
#Write for python use
write.csv2(unique(data1$ZIPCODE), file="zipcodes.csv", sep=",")

##Insert weather data

weather_data <- fread('./data/raw/Iowa_weather.csv', header = T, sep = ',', verbose=TRUE)
str(weather_data)
summary(weather_data)

weather_data <- select(weather_data, -STATION, -TSUN, -AWND, -WT09, -WT01, -WT06, -WT05, -WT02, -WT11, -WT04, -WT08, -WT03)
weather_data$ELEVATION <- as.numeric(weather_data$ELEVATION)
weather_data$LATITUDE <- as.numeric(weather_data$LATITUDE)
weather_data$LONGITUDE <- as.numeric(weather_data$LONGITUDE)
#lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
weather_data$DATE <- as.Date(as.character(weather_data$DATE), "%Y%m%d")
#Celsius convert. range of T is -200 to 200; something wrong
#weather_data$TMIN <- (weather_data$TMIN-32)*5/9

#Insert zipcode and lat/lon data
zip_lat_lon_data <- fread('./data/processed/ZIPCODES_LATLON.csv', header = T, sep = ',', verbose=TRUE)
str(zip_lat_lon_data)


weather_data <- left_join(weather_data, zip_lat_lon_data, by=c("LATITUDE", "LONGITUDE"))
weather_data <- select(weather_data, -V1)
#NAs in ZIPCODE. Possibly latitude and longitude not exactly same. Trying to populate based on min distance.
a1 <- weather_data$LATITUDE[is.na(weather_data$ZIPCODE)]
a2 <- weather_data$LONGITUDE[is.na(weather_data$ZIPCODE)]
a3 <- data.frame(a1,a2)
a3 <- a3[!duplicated(a3),]
a3 <- a3[c(2,1)]
a3 <- na.omit(a3)
b1 <- select(zip_lat_lon_data, LATITUDE, LONGITUDE, ZIPCODE)
b2 <- as.data.frame(select(zip_lat_lon_data, LATITUDE, LONGITUDE, ZIPCODE))
b2 <- b2[c(2,1,3)]
install.packages('geosphere')
library("geosphere")
#Code to add zipcode in intermediate a3 data.frame that had NA's. To be merged to data1.
for (index1 in seq(1,nrow(a3))) {
  b <- c()
  for (index2 in seq(1,nrow(b2))) {
    a <- distGeo(a3[index1,], b2[index2, 1:2])
    b <- append(a, b)
  }
  a3[index1,3] <- b2[which.min(b),3]
}
colnames(a3)[1] <- "LONGITUDE"
colnames(a3)[2] <- "LATITUDE"
colnames(a3)[3] <- "ZIPCODE"
a3 <- a3[c(2,1,3)]

#Merge to master dataset
cond <- (is.na(weather_data$ZIPCODE))&(!is.na(weather_data$LATITUDE))
#Remove zipcode na's from
weather_data3 <- select(weather_data[cond], -ZIPCODE)
weather_data3 <- left_join(weather_data3,a3, by=c("LATITUDE", "LONGITUDE"),copy=TRUE)
weather_data4 <- weather_data[!cond]
weather_data <- rbind(weather_data3, weather_data4)
setnames(weather_data, "LONGITUDE", "W_LONGITUDE")
setnames(weather_data, "LATITUDE", "W_LATITUDE")
setnames(weather_data, "STATION_NAME", "W_STATION_NAME")
weather_data <- select(weather_data, c(DATE, ZIPCODE, PRCP, SNOW, TMIN, TMAX))
#Convert to Celsius
weather_data$TMIN <- (weather_data$TMIN/10 - 32)*0.5556
weather_data$TMAX <- (weather_data$TMAX/10 - 32)*0.5556

#Final merge weather data to master
inters_zipcodes <- intersect(unique(data1$ZIPCODE), unique(weather_data5$ZIPCODE))

data1a <- data1[which(data1$ZIPCODE==inters_zipcodes)]
data1b <- data1[!which(data1$ZIPCODE==inters_zipcodes)]
#Initialise columns for rbind
data1b$PRCP <- NA
data1b$SNOW <- NA
data1b$TMIN <- NA
data1b$TMAX <- NA
#Remove duplicate zipcode-date entries. These are valid as they are from different stations
weather_data <- weather_data[!duplicated(weather_data, by=c("DATE","ZIPCODE"))]
data1a <- left_join(data1a, weather_data, by=c("DATE", "ZIPCODE"))
#Final
data1 <- rbind(data1a, data1b)
#Import nationalities data scraped from zipatlas
nationalities <- c('arab', 'asian','black','chinese','czech','danish','dutch','english','filipino',
                   'french','german','greek','hispanic','indian','irish','italian','japanese','korean','lithuanian',
                   'mexican','native','norwegian','polish','portuguese','russian','scottish','slovak','swedish','swiss',
                   'welsh','west','white')
sub_nationalities <- nationalities[c(1:4, 7,8, 10:17, 20,21,23, 25,26,32)]
data1 <- data1[,c(1:22, 46:51)]
for (nation in sub_nationalities){
  assign(nation, fread(paste('./data/processed/',
                             paste(nation,".csv", sep=""), sep=""),
                             header = T, sep = ',', verbose=TRUE))
  n = get(nation)
  setnames(n, colnames(n)[3], "ZIPCODE")
  setnames(n, colnames(n)[7], paste("Perc_",nation,sep=""))
  setnames(n, colnames(n)[8], paste("NRank_",nation, sep=""))
  n <- n[,c(3,7,8), with=FALSE]
  n <- n[(n$ZIPCODE %in% unique(data9$ZIPCODE)),]
  n <- as.data.table(sapply(n,gsub,pattern="[,#%]",replacement=""))
  n <- as.data.table(sapply(n, function(x) as.numeric(as.character(x))))
  assign(nation, n)
  data1 <- left_join(data1, get(nation), by="ZIPCODE")
}

#Normalise liquor quantities by population
data1$BOTTLE_QTY_NORM <- NA
data1$TOTAL_NORM <- NA
data1$TOTAL_EST_NORM <- NA
data1$`BOTTLE_QTY_NORM` <- data1$'BOTTLE QTY'/data1$POPULATION
data1$TOTAL_NORM <- data1$TOTAL/data1$POPULATION
data1$TOTAL_EST_NORM <- data1$TOTAL_EST/data1$POPULATION

