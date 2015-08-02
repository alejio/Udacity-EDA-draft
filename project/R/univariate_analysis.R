library(ggplot2)
pmonth <- c("January", "February", "March", "April", "May", "June", "July", "August",
            "September", "October", "November", "December")
pday <- c("Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

#####Univariate plots

# Bottle sales data --------------------------------------------------------


###Transaction data
##Bottles sold
qplot(data1$`BOTTLE QTY`)
#Not easy to see shape of distribution. Looking at log-transform
bks <- seq(min(data1$`BOTTLE QTY`, na.rm=T), max(data1$`BOTTLE QTY`, na.rm=T))
ggplot(data=data1, aes(x=`BOTTLE QTY`)) +
  geom_histogram(aes(y =..density..),
                 col="red",
                 fill="green",
                 alpha = .2, origin = -0.5) +
  labs(title="Histogram for Log Bottle Quantities") +
  labs(x="Bottles", y="Count") +  scale_x_log10(breaks=bks, labels=NULL)

#Let's focus to the denser area of the distribution
ggplot(data=data1, aes(x=`BOTTLE QTY`)) +
  geom_histogram(aes(y =..density..),
                 col="red",
                 fill="green",
                 alpha = .2, origin = -0.5, binwidth=1) +
  labs(title="Histogram for Bottle Quantities") +
  labs(x="Bottles", y="Count") +  scale_x_continuous(limits=c(0,50), breaks=seq(0,50,2))

#Most sales are small in quantity. Multiples of 12 are high occurring.
quantile(data1$`BOTTLE QTY`)
quantile(data1$`BOTTLE QTY`, seq(0.95,1,0.01))
quantile(data1$`BOTTLE QTY`, seq(0.99,1,0.001))
#Outliers are also present; 1% of observations between 60 and 3456 and 0.1% between 300 and 3456.
length(data1$`BOTTLE QTY`[data1$`BOTTLE QTY`>1000])
#There are 106 cases where above 1000 bottles were purchased.
table(data1[data1$`BOTTLE QTY`>1000,]$ZIPCODE)
ggplot(data=data1[data1$`BOTTLE QTY`>1000,], aes(x=ZIPCODE)) +
  geom_histogram(col="red",
                 fill="green",
                 alpha = .2, origin = -0.5, binwidth=1) +
  labs(title="Histogram for Zipcodes with large orders") +
  labs(x="Zipcode", y="Count")

ggplot(data=data1[data1$`BOTTLE QTY`>1000,], aes(x=`DATE`, y=`BOTTLE QTY`))+
  geom_point(aes(size=POPULATION, color=ZIPCODE))









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

