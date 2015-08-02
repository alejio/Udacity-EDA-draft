require("RSQLite")
# Set up database
drv <- dbDriver("SQLite")
tfile <- "Iowa.db"
con <- dbConnect(drv, dbname = "Iowa.db")
dbWriteTable(con, "data1", as.data.frame(data1))
dbDisconnect(con)
