left_join.nationalities <- function(x, df){
  # Read in data set for nationality x percentages
  assign(x, fread(paste('./data/processed/',
                             paste(x,".csv", sep=""), sep=""),
                       header = T, sep = ',', verbose=TRUE))
  # Get name
  nation=get(x)
  # Pre-process df; set names
  setnames(nation, colnames(nation)[c(3,7,8)],
           c("ZIPCODE", paste("Perc_", x, sep=""),
             paste("NRank_", x, sep="")))
  # Drop unwanted columns
  nation <- nation[, c(3,7,8), with=F]
  # Only keep entries that have corresponding in main data
  nation <- nation[(nation$ZIPCODE %in% unique(df$ZIPCODE)),]
  # Remove unwanted characters (e.g. %)
  nation <- as.data.table(sapply(nation, gsub, 
                                 pattern="[,#%]", replacement=""))
  # Make numeric
  nation <- as.data.table(sapply(nation,
                                 function(y) as.numeric(
                                   as.character(y))))
  assign(x, nation)
  # Join to main data table
  df <- left_join(df, get(x), by="ZIPCODE")
}