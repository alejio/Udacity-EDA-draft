#Function from http://www.r-bloggers.com/boxplot-with-mean-and-standard-deviation-in-ggplot2-plus-jitter/
#calculates stats to add as layer to ggplot2
min.mean.sd.max <- function(x) {
  r <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

#ggplot functions
ggplot(data=data, aes((data$"VENDOR")))+geom_histogram(binwidth=1) +
  scale_x_discrete(labels=abbreviate) + theme(axis.text.x  = element_text(angle=90, size=6))


p1 <- ggplot(data=subset(data1, ITEM==11788), aes(x=DATE, y=BOTTLE_QTY_NORM))+ xlim(data1$DATE[100], data1$DATE[100]+15)
p1 <- p1 + stat_summary(fun.data = min.mean.sd.max, geom = "boxplot")
p1 + geom_point()
+ geom_jitter(position=position_jitter(width=.2), size=3)


a1 = rnorm(10)
a2 = rnorm(25)
a3 = rnorm(17)
a_list = list(a1, a2, a3)
a_df = do.call("rbind", lapply(a_list,
                               function(x) data.frame(value = x,
                                                      count = seq_along(x))))
ID_options = LETTERS[seq_along(a_list)]
sapply(a_list, length)
a_df$ID = rep(ID_options, sapply(a_list, length))
ggplot(a_df, aes(x = value, y = count, color = ID)) + geom_point()

a1 <- data.frame(a = rnorm(10), c = rnorm(10), g = rnorm(10), class = sample(letters[20:23], 10, TRUE))
a1.m <- melt(a1)

overlay_scatter <- function(df, vars, colvar){
  tot_vars <- length(vars)
  test <- rbind(df[df[,colvar]=="100 PROOF VODKA",],data=gr_date_cat[gr_date_cat[,2]=="80 PROOF VODKA",])

}
test <- rbind(gr_date_cat[gr_date_cat[,2]=="100 PROOF VODKA",],data=gr_date_cat[gr_date_cat[,2]=="80 PROOF VODKA",])
test.m <- melt(test, id.vars = "DATE", measure.vars = "sum_TOTAL")
test.m <- melt(test, id.vars = c("CATEGORY NAME","DATE"), measure.vars="sum_BOTTLE_QTY")
ggplot(test.m, aes(DATE, value, colour = `CATEGORY NAME`)) +
  geom_point()

test <- gr_date_cat[gr_date_cat$`CATEGORY NAME` %in% c("TEQUILA", "TRIPLE SEC"),1:3]
test <- spread(test,`CATEGORY NAME`, sum_BOTTLE_QTY)
ggplot(data=test, aes(x=TEQUILA, y=`TRIPLE SEC`)) + geom_point()

library("GGally")
ggpairs()

CenterOfMap <- geocode("41.9137948,-93.3293731")
Iowa <- get_map(c(lon=CenterOfMap$lon, lat=CenterOfMap$lat), zoom = 7, maptype = "hybrid", source = "google")
IowaMap <- ggmap(Iowa)
IowaMap 

IowaMap + geom_point(aes(x = Longitude, y = Latitude, size=sqrt(estArea)),
           data = data, alpha = .5, color="darkred")