
# Load and pre-process liquor sales data ----------------------------------

#Using amazingly fast fread function to read in 800MB csv file
library(data.table)
data1 <- fread('./data/raw/Iowa_Liquor_Sales.csv', header = T, sep = ',',
               verbose=TRUE)
#Convert dates to Date objects
data1$DATE <- as.Date(data1$DATE, "%m/%d/%Y")

#Fix ZIPCODE issues
data_zipcodes <- unique(data1$ZIPCODE)
#Rogue zipcodes are 97 (sioux city, Morningside avenue) and NA (Dunlap).
##Dunlap is 61525 and Sioux city is 51106 (googled it)
summary(data_zipcodes)
min(na.omit(data_zipcodes))
max(na.omit(data_zipcodes))
sum(is.na(data1$ZIPCODE))
data1$CITY[is.na(data1$ZIPCODE)]
data1$ZIPCODE[is.na(data1$ZIPCODE)] <- 61525
data1$CITY[data1$ZIPCODE==97]
data1$ZIPCODE[data1$ZIPCODE==97] <- 51106

#Convert ZIPCODE to unordered factor (nominal variable)
data1$ZIPCODE <- factor(data1$ZIPCODE, ordered=F)

#Omit dollars in cost and price variables and convert to numeric;
#could have used gsub
data1$`STATE BTL COST` <- sapply(strsplit(data1$`STATE BTL COST`, 
                                          split='$', fixed=TRUE), 
                                 function(x) (x[2]))
data1$`STATE BTL COST` <- as.numeric(data1$`STATE BTL COST`)
data1$`BTL PRICE` <- sapply(strsplit(data1$`BTL PRICE`, split='$',
                                     fixed=TRUE), function(x) (x[2]))
data1$`BTL PRICE` <- as.numeric(data1$`BTL PRICE`)
data1$`TOTAL` <- sapply(strsplit(data1$`TOTAL`, split='$', fixed=TRUE),
                        function(x) (x[2]))
data1$`TOTAL` <- as.numeric(data1$`TOTAL`)

#The STORE LOCATION variable holds latitude and longitude values
#These will be useful in plotting on maps so let's extract them.
data1$STORE_LAT <- sapply(data1$`STORE LOCATION`, extract_lat_lon)[1,]
data1$STORE_LON <- sapply(data1$`STORE LOCATION`, extract_lat_lon)[2,]

#Dropping STORE LOCATION as it is redundant
select(data1, -`STORE LOCATION`)

#2015 data are not for the full year so I will just keep 2014
data1 <- subset(data1, DATE<"2015-01-01")