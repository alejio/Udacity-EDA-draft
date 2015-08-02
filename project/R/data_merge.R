###Here the main and auxilliary data sets are merged

# 1. Merge main and auxilliary establishment data set ---------------------

data1 <- left_join(data1, est_data, by = "ZIPCODE")
# NA's present because zipcode was not in est_data3
data1 <- na.omit(data1)
#Calculate total establishments
data1$TOTAL_EST <- rowSums(data1[seq(0,nrow(data1)), 23:45,with=FALSE])

# 2. Merge main and auxilliary population data set ------------------------

# Merge population data with base
data1 <- left_join(data1, pop_data, by = "ZIPCODE")
# Write unique ZIPCODE for Python use
#write.csv2(unique(data1$ZIPCODE), file="zipcodes.csv", sep=",")

# 3. Merge main and weather data set --------------------------------------
# Firstly identify common zipcodes in two data sets
inters_zipcodes <- intersect(unique(data1$ZIPCODE),
                             unique(weather_data5$ZIPCODE))

# Split main data set based on zipcode intersection
data1.paired <- data1[which(data1$ZIPCODE==inters_zipcodes)]
data1.unpaired <- data1[!which(data1$ZIPCODE==inters_zipcodes)]
# Initialise columns for rbind
data1.unpaired$PRCP <- NA
data1.unpaired$SNOW <- NA
data1.unpaired$TMIN <- NA
data1.unpaired$TMAX <- NA
# Remove duplicate zipcode-date entries. These are valid as they are from
# different stations
weather_data <- weather_data[!duplicated(weather_data,
                                         by=c("DATE","ZIPCODE"))]
data1.paired <- left_join(data1.paired, weather_data,
                          by=c("DATE", "ZIPCODE"))
#Re-join unpaired with enriched paired
data1 <- rbind(data1.paired, data1.unpaired)


# 4. Merge main and demographics data sets --------------------------------

# I made these data sets by scraping zipatlas tables in Python
# example URL:
# http://zipatlas.com/us/ia/zip-code-comparison/
# percentage-white-population.htm

# Nationalities data sets name parts
nationalities <- c('arab', 'asian','black','chinese','czech','danish',
                   'dutch','english','filipino', 'french','german','greek',
                   'hispanic','indian','irish','italian','japanese','korean',
                   'lithuanian', 'mexican','native','norwegian','polish',
                   'portuguese','russian','scottish','slovak','swedish',
                   'swiss', 'welsh','west','white')
# Which ones to keep in the end
sub_nationalities <- nationalities[c(1:4, 7,8, 10:17, 20,21,23, 25,26,32)]
# So, because of data set size, I decided to omit individual establishments
#data and only keep totals...
data1 <- data1[,c(1:22, 46:51)]
# Run left_join.nationalities program that does all pre-processing and
#joining to main. 
for (nation in sub_nationalities){
  left_join.nationalities(nation, data1)
}
