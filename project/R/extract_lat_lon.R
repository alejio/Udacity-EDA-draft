extract_lat_lon <- function(x){
  #Extract latitude and longitude from complex string
  #Example: 110 S 5TH ST\nADAIR 50002\n(41.498071787000072, -94.643396801999984)
  #Split on \n and keep first (and only?) list of lists
  a <- strsplit(x, split="\n")[[1]]
  #Keep last element representing lat and lon
  a <- a[length(a)]
  #Omit parentheses; maybe a regex would be better
  a <- substr(a, 2, nchar(a)-1)
  #Split on comma
  a <- strsplit(a, split=",")
  #convert to list with two elements; lat first and lon second
  a <- as.numeric(a[[1]])
  return(a)
}