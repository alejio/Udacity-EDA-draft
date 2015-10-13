library(ggplot2)
pmonth <- c("January", "February", "March", "April", "May", "June", "July", "August",
            "September", "October", "November", "December")
pday <- c("Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

#####Univariate plots

# Bottle sales data --------------------------------------------------------


###Transaction data
##Bottles sold
ggplot(data=data1, aes(x=`BOTTLE QTY`)) +
  geom_histogram(aes(y =..density..),
                 fill=I("blue"), col=I("black"), alpha=0.5, origin = -0.5) +
  labs(title="Histogram for Bottle Quantities", x="Bottles", y="Count") +
  theme(panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(),
        plot.title = element_text(size = rel(1.5), colour = "blue"))
#Not easy to see shape of distribution. Looking at log-transform
bks <- seq(min(data1$`BOTTLE QTY`, na.rm=T), max(data1$`BOTTLE QTY`, na.rm=T))
ggplot(data=data1, aes(x=`BOTTLE QTY`)) +
  geom_histogram(aes(y =..density..),
                 fill=I("blue"), col=I("black"), alpha=0.5, origin = -0.5) +
  labs(title="Histogram for Log Bottle Quantities", x="Bottles", y="Count") +
  scale_x_log10(breaks=bks, labels=NULL) +
  theme(panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(),
        plot.title = element_text(size = rel(1.5), colour = "blue"))
 
#Let's focus on the denser area of the distribution

ggplot(data=data1, aes(x=`BOTTLE QTY`)) +
  geom_histogram(aes(y =..density..),
                 fill=I("blue"), col=I("black"), alpha=0.5, origin = -0.5) +
  labs(title="Histogram for Bottle Quantities", x="Bottles", y="Count") +
  theme(panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.ticks=element_blank(),
        plot.title = element_text(size = rel(1.5), colour = "blue")) + 
  scale_x_continuous(limits=c(0,50), breaks=seq(0,50,2))

#Most sales are small in quantity. Multiples of 12 are high occurring.
quantile(data1$`BOTTLE QTY`)
quantile(data1$`BOTTLE QTY`, seq(0.95,1,0.01))
quantile(data1$`BOTTLE QTY`, seq(0.99,1,0.001))
#Outliers are also present; 1% of observations between 60 and 3456 and 0.1% between 300 and 3456.
length(data1$`BOTTLE QTY`[data1$`BOTTLE QTY`>1000])
#There are 106 cases where above 1000 bottles were purchased.

ggplot(data=data1[data1$`BOTTLE QTY`>1000,], aes(x=`DATE`, y=`BOTTLE QTY`))+
  geom_point(aes(size=POPULATION, color=`COUNTY`))


# Focus on first part of year -------------------------------------------------
#Add a population normalised bottles variable
gr_date.Jan_May <- ddply(filter(data1, (DATE < "2014-06-01")),
                     .(DATE), summarise, BOTTLES = sum(`BOTTLE QTY`, na.rm=T), 
                     BOTTLES_NORM = sum(`BOTTLE QTY`/POPULATION, na.rm=T))

# Reshape data for ggplot
test <- gr_date.Jan_May
maxbot <- max(gr_date.Jan_May$BOTTLES, na.rm=T)
maxbotnorm <- max(gr_date.Jan_May$BOTTLES_NORM, na.rm=T)
test<- arrange(gather(test, "type", "bottles", 2:3), DATE)
test <- mutate(test, maxbottles =ifelse(type=="BOTTLES", maxbot, maxbotnorm))
gr_date.Jan_May <- test
rm(test)

#Plot time series in same plot
ggplot(data=gr_date.Jan_May,aes(x=DATE, y=bottles/maxbottles, colour=type)) + 
  geom_point()+ geom_line() +
  geom_ribbon(aes(ymin=0, ymax=bottles/maxbottles, fill=type)) +
  scale_fill_brewer(palette="BrBG")+
  labs(x="Date", y = "Bottles")+
  theme(panel.background = element_rect(fill = 'white'),
        plot.title = element_text(size = rel(1), colour = "blue")) 

# Cross-plotting the data
ggplot(data=gr_date.Jan_May,aes(x=bottles[type=="BOTTLES"]/maxbottles[type=="BOTTLES"], 
                                y=bottles[type=="BOTTLES_NORM"]/maxbottles[type=="BOTTLES_NORM"],
                                shape=weekdays(DATE[type=="BOTTLES"]),
                                colour=weekdays(DATE[type=="BOTTLES"]))) +
  geom_point(size=3)

#useful plotting code
test1 <- ggplot(data=gr_date.Jan_May, aes(x=DATE, y=BOTTLES)) + geom_line()
test2 <- ggplot(data=gr_date.Jan_May, aes(x=DATE, y=BOTTLES_NORM)) + geom_line() +geom_point()
grid.newpage()
grid.draw(rbind(ggplotGrob(test1), ggplotGrob(test2), size="last"))

#Normalised bottles; clear periodicity!
p1 <- ggplot(data=filter(gr_date.Jan_May, type=="BOTTLES_NORM"),
             aes(x=factor(weekdays(DATE), levels=day.names),
                 y=bottles)) +geom_boxplot()+
  labs(x="Weekday", y="Bottles (pop bias removed)")

p2 <- ggplot(data=filter(gr_date.Jan_May, type=="BOTTLES_NORM"), 
             aes(x=format(DATE, "%U"),
                 y=bottles)) +geom_boxplot()+
  labs(x="Week", y="Bottles (pop bias removed)")

p3 <- ggplot(data=filter(gr_date.Jan_May, type=="BOTTLES_NORM"),
             aes(x=factor(months(DATE), levels=month.name),
                 y=bottles)) +geom_boxplot()+
  labs(x="Month", y="Bottles (pop bias removed)")
multiplot(p1, p2, p3, cols=2)
rm(p1,p2,p3)


# Individual categories -----------------------------------------------------

gr_date_cat2 <- ddply(data1, .(DATE, `CATEGORY NAME`), summarise, 
                      BOTTLES = sum(`BOTTLE QTY`, na.rm=T))

gr_date_cat2.month <- ddply(gr_date_cat2, .(factor(months(DATE), levels=month.name),
                                    `CATEGORY NAME`), summarise,
                            BOTTLES = sum(`BOTTLES`, na.rm=T))
setnames(gr_date_cat2.month, 1, "Month")
gr_date_cat3 <- ddply(gr_date_cat2.month, .(Month), summarise,
                      TOT_BOTT = sum(BOTTLES, na.rm=T))

gr_date_cat2.month  <- left_join(gr_date_cat2.month, gr_date_cat3)
gr_date_cat2.month $BCAT_NORM <- gr_date_cat2.month $BOTTLES/gr_date_cat2.month $TOT_BOTT
setnames(gr_date_cat2.month , 2, "CATEGORY")

ggplot(data=gr_date_cat2.month , aes(x=Month, y=BCAT_NORM, group=1)) +
  facet_wrap(~ CATEGORY, scales = "free_y") +
  labs(x="Month", y = "Normalised bottle sales", 
       title = "Liquor categories bottle sales with season overlay") + 
  theme(panel.background = element_rect(fill = 'white'),
        strip.text = element_text(size=8, colour="blue"),
        strip.background = element_rect(fill="white"),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks=element_blank()) +
  geom_rect(data=NULL,aes(xmin="January",xmax="March",ymin=-Inf,ymax=Inf),
            fill="royalblue1", alpha=0.5) +
  geom_rect(data=NULL,aes(xmin="March",xmax="June",ymin=-Inf,ymax=Inf),
            fill="greenyellow", alpha=0.5) +
  geom_rect(data=NULL,aes(xmin="June",xmax="September",ymin=-Inf,ymax=Inf),
            fill="tomato", alpha=0.5) +
  geom_rect(data=NULL,aes(xmin="September",xmax="December",ymin=-Inf,ymax=Inf),
            fill="violet", alpha=0.5) +
  geom_rect(data=NULL,aes(xmin="December",xmax=Inf,ymin=-Inf,ymax=Inf),
            fill="royalblue1", alpha=0.5) +
  geom_line()

ggplot(data=test, aes(x=Month, y=BCAT_NORM, group=1)) +
  theme(panel.background = element_rect(fill = 'white'),
        strip.text = element_text(size=8, colour="blue"),
        axis.text.y = element_text(size=5),
        axis.text.x = element_blank()) +
  geom_rect(data=NULL,aes(xmin="January",xmax="March",ymin=-Inf,ymax=Inf),
            fill="royalblue1") +
  geom_rect(data=NULL,aes(xmin="March",xmax="June",ymin=-Inf,ymax=Inf),
            fill="greenyellow") +
  geom_rect(data=NULL,aes(xmin="June",xmax="September",ymin=-Inf,ymax=Inf),
            fill="tomato") +
  geom_rect(data=NULL,aes(xmin="September",xmax="December",ymin=-Inf,ymax=Inf),
            fill="violet") +
  geom_rect(data=NULL,aes(xmin="December",xmax=Inf,ymin=-Inf,ymax=Inf),
            fill="royalblue1")+ geom_line()
  



ggplot(data=filter(test, CATEGORY=="100 PROOF VODKA"), aes(x=factor(months(DATE), levels=month.name),
                      y=BCAT_NORM)) +geom_boxplot()
  facet_wrap(~ CATEGORY)

# Overlaying on to map
#use ggmap and RgoogleMaps
#ggmap cheatsheet very useful
#https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/ggmap/ggmapCheatsheet.pdf
#Define centre of map
CenterOfMap <- geocode("41.9137948,-93.3293731")
# Get the map
#Iowa <- get_map(c(lon=CenterOfMap$lon, lat=CenterOfMap$lat), zoom = 7,
#                maptype = "roadmap", source = "google")

Iowa <- get_map(c(lon=CenterOfMap$lon, lat=CenterOfMap$lat), zoom = 7,
                  source = "stamen", maptype= "toner", color="bw")
#use geocodeQueryCheck() to check how many I've got left
# Plot the map
IowaMap <- ggmap(Iowa)
# Plot points on map
#Top 10 occurring liquor transactions (categories)
top10.liquor.trans <- rownames(sort(table(data1$`CATEGORY NAME`), 
                                    decreasing=T)[1:10])
IowaMap + geom_point(aes(x = STORE_LON, y = STORE_LAT, size=`BOTTLE QTY`,
                         color=`CATEGORY NAME`), 
                     data = filter(data1, `CATEGORY NAME` %in% top10.liquor.trans))


##Total money spent per transaction
qplot(data1$TOTAL)
qplot(data1$TOTAL) + xlim(0,1000)
#Back to univariate
##Now is the time to start grouping
##Group by date
gr_date <- ddply(data1, .(DATE), summarise, sum_BOTTLE_QTY=sum(`BOTTLE QTY`, na.omit=T), sum_TOTAL=sum(TOTAL, na.omit=T))
qplot(data=gr_date, x=DATE, y=sum_BOTTLE_QTY) + geom_line()
qplot(data=gr_date, x=DATE, y=sum_TOTAL) + geom_line()
qplot(data=gr_date, x=sum_BOTTLE_QTY, y=sum_TOTAL)+ geom_abline(slope=13.5)

#Group by store
gr_store <- ddply(data1, .(STORE), summarise, 
                 sum_BOTTLE_QTY=sum(`BOTTLE QTY`, na.omit=T),
                 population=mean(POPULATION, na.omit=T), lon=mean(STORE_LON, na.rm=T),
                 lat=mean(STORE_LAT, na.rm=T))


# Plot points on map along with unis

IowaMap + 
  geom_point(data = gr_store, aes(x = lon, y = lat, size=`sum_BOTTLE_QTY`)) + 
  geom_point(data=unis, aes(x=lon, y=lat, size=1+enrolled), colour="red", shape=7)


#Group by both date and category
gr_date_cat <- ddply(data1, .(DATE, `CATEGORY NAME`), summarise, sum_BOTTLE_QTY=sum(`BOTTLE QTY`, na.omit=T), sum_TOTAL=sum(TOTAL, na.omit=T))
qplot(data=gr_date_cat[gr_date_cat[,2]=="100 PROOF VODKA",], x=DATE, y=sum_BOTTLE_QTY)+ geom_line()
qplot(data=gr_date_cat[gr_date_cat[,2]=="100 PROOF VODKA",], x=DATE, y=sum_BOTTLE_QTY)+ geom_line()
#Clearly there is an abnormality occurring around June.
#Group by category
gr_cat <- ddply(data1, .(`CATEGORY NAME`), summarise, sum_BOTTLE_QTY=sum(`BOTTLE QTY`, na.omit=T), sum_TOTAL=sum(TOTAL, na.omit=T))
#Get top 5 categories
top_selling_liquors <- setorder(as.data.table(gr_cat), -sum_BOTTLE_QTY)
#Time series scatterplot of liquor sales per category
ggplot(data=gr_date_cat[gr_date_cat$`CATEGORY NAME` %in% top_selling_liquors$`CATEGORY NAME`
                        [1:2],], aes(x=DATE, y=sum_BOTTLE_QTY, colour=`CATEGORY NAME`))+geom_point()

# group by item
gr_item <- ddply(data1, .(DESCRIPTION), summarise,
                 bottles=sum(`BOTTLE QTY`, na.rm=T), price=mean(`BTL PRICE`))
#plot raw
ggplot(data=gr_item, aes(x=bottles, y=price)) + geom_point()
#plot log-transformed
#good relationship
ggplot(data=gr_item, aes(x=bottles, y=price)) + geom_point() +
  scale_y_log10() + scale_x_log10() + geom_smooth(method=lm, fill="red")

#group by category
gr_cat <- ddply(data1, .(`CATEGORY NAME`), summarise,
                           bottles=sum(`BOTTLE QTY`, na.rm=T), price=mean(`BTL PRICE`))
ggplot(data=gr_cat, aes(x=bottles, y=price)) + geom_point() + scale_x_log10() + scale_y_log10()
#Dead end