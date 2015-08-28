# Auditing reporting ------------------------------------------------------
#Monthly

#make DF summarising reports for each store each month
gr_st_month_count <- ddply(data1, .(months(DATE), STORE), summarise, 
              count_reps=length(STORE))
setnames(gr_st_month_count,1, "month")
gr_st_month_count$month <- factor(gr_st_month_count$month,
                                  levels=month.name)

# Make separate DF counting number of participating stores per month
gr_st_month.part_stores <- data.frame("month"=factor(rownames(
  table(gr_st_month_count$month)),levels=month.name),
  "part_stores"=as.numeric(table(gr_st_month_count$month)))

#Combine data sets for ggplot use
gr_st_month_count <- left_join(gr_st_month_count, gr_st_month.part_stores)
rm(gr_st_month.part_stores)

#Create plots for comparing store report frequency
#Plot1: number of reports per month and num. of participating stores overlay
ggplot(data=gr_st_month_count, aes(x=month, y=count_reps)) + 
  stat_boxplot(geom='errorbar') +
  geom_boxplot() +
  geom_point(aes(x=month, y=part_stores), shape=23, size=7, 
             colour="red", fill="yellow") +
  geom_line(aes(x=month, y=part_stores, group=1), linetype="dashed")

#Plot2: same as plot 1 but using boxplot colour as part. stores variable
ggplot(data=gr_st_month_count, aes(x=month, y=count_reps)) + 
  stat_boxplot(geom='errorbar') + geom_jitter(alpha=0.2) + 
  geom_boxplot(aes(fill=part_stores), outlier.size=0) +
  coord_cartesian(ylim = c(0, 1000)) +
  theme(panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.title = element_text(size = rel(1.5), colour = "blue")) + 
  labs(x="Month", y = "Transactions", 
title = "Monthly Transactions between Iowa State and Liquor Stores")



# Weekly reports ----------------------------------------------------------

# Get weekly reports as per month methodology

gr_st_week_count <- ddply(data1, .(format(DATE, "%U"), STORE), summarise, 
                             count_reps=length(STORE))
setnames(gr_st_week_count,1, "week")
gr_st_week_count$week <- as.numeric(gr_st_week_count$week)
gr_st_week.part_stores <- data.frame("week"=rownames(table(gr_st_week_count$week)),
  "part_stores"=as.numeric(table(gr_st_week_count$week)))
gr_st_week.part_stores$week <- as.numeric(as.character(gr_st_week.part_stores$week))
gr_st_week_count <- left_join(gr_st_week_count, gr_st_week.part_stores)
#changed my mind. will make week factor
gr_st_week_count$week <- factor(gr_st_week_count$week)
rm(gr_st_week.part_stores)

#Plot: same as plot 1 but using boxplot colour as part. stores variable
ggplot(data=gr_st_week_count, aes(x=week, y=count_reps)) + 
  stat_boxplot(geom='errorbar') + geom_jitter(alpha=0.2) + 
  geom_boxplot(aes(fill=part_stores), outlier.size=0) +
  coord_cartesian(ylim = c(0, 250)) +
  theme(panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.title = element_text(size = rel(1.5), colour = "blue")) + 
  labs(x="Week", y = "Transactions", 
       title = "Weekly Transactions between Iowa State and Liquor Stores")


# Daily reports -----------------------------------------------------------

gr_st_day_count <- ddply(filter(data1, (DATE <= "2014-06-08") & 
                                  (DATE >="2014-05-25")), .(DATE, STORE),
                         summarise, count_reps=length(STORE))
setnames(gr_st_day_count,1, "day")
gr_st_day.part_stores <- data.frame("day"=rownames(table(gr_st_day_count$day)),
                                     "part_stores"=as.numeric(table(gr_st_day_count$day)))
gr_st_day.part_stores$day <- as.Date(as.character(gr_st_day.part_stores$day))
gr_st_day_count <- left_join(gr_st_day_count, gr_st_day.part_stores)
rm(gr_st_day.part_stores)

#Plot: same as plot 1 but using boxplot colour as part. stores variable
ggplot(data=gr_st_day_count, aes(x=as.character(day), y=count_reps)) + 
  stat_boxplot(geom='errorbar') + geom_jitter(alpha=0.2) + 
  geom_boxplot(aes(fill=part_stores), outlier.size=0) +
  coord_cartesian(ylim = c(0, 250)) +
  theme(panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.title = element_text(size = rel(1.5), colour = "blue")) + 
  labs(x="Day", y = "Transactions", 
       title = "Daily Transactions between Iowa State and Liquor Stores")


# Reporting on days of week -----------------------------------------------

#Weekday
gr_st_weekday_count <- ddply(data1, .(weekdays(DATE), STORE), summarise, 
                             count_reps=length(STORE))
setnames(gr_st_weekday_count,1, "weekday")
day.name <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
              "Friday", "Saturday", "Sunday")
gr_st_weekday_count$weekday <- factor(gr_st_weekday_count$weekday, levels=day.name)
gr_st_weekday.part_stores <- data.frame("weekday"=rownames(table(gr_st_weekday_count$weekday)),
                                    "part_stores"=as.numeric(table(gr_st_weekday_count$weekday)))
gr_st_weekday.part_stores$weekday <- factor(gr_st_weekday.part_stores$weekday, levels=day.name)
gr_st_weekday_count <- left_join(gr_st_weekday_count, gr_st_weekday.part_stores)
rm(gr_st_weekday.part_stores)

ggplot(data=gr_st_weekday_count, aes(x=weekday, y=count_reps)) + 
  stat_boxplot(geom='errorbar') + geom_jitter(alpha=0.2) + 
  geom_boxplot(aes(fill=part_stores), outlier.size=0) +
  coord_cartesian(ylim = c(0, 4500)) +
  theme(panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.title = element_text(size = rel(1.5), colour = "blue")) + 
  labs(x="Weekday", y = "Transactions", 
       title = 
         "Transactions between Iowa State and Liquor Stores per weekday")


# What happens in June? ---------------------------------------------------
#Subset data for week 21 and week 22 and calculate reports variation per store
gr_st_month_count.0506 <- filter(gr_st_month_count, (month=="May")|(month=="June"))
gr_st_month_count.0506 <- select(gr_st_month_count.0506, -part_stores)
#library(tidyr)
gr_st_month_count.0506 <- spread(gr_st_month_count.0506, month, count_reps)
ggplot(data = gr_st_month_count.0506, aes(x=100*(June-May)/May)) + 
  geom_histogram(fill=I("blue"), col=I("black"), alpha=0.5) +
  geom_vline(linetype="dashed", color="red") +
  theme(panel.background = element_rect(fill = 'white'),
        plot.title = element_text(size = rel(1.5), colour = "blue")) + 
  labs(x = "Percentage of change between May - June 14", 
       title = "Percentage of change in transactions between May/June 14 per store")
