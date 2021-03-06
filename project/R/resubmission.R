# plot distributions instead of showing numbers
# Function for original liquor sales variables
# y is column number of master df, x is binwidth for hist
quick_hist <- function(y, x = 0) {
  npl <- y-13
  test =data1[, y, with=F]
  test <- na.omit(test)
  x = ifelse(x==0, (max(test)-min(test))/30, x)
  nameold <- colnames(test)[1]
  namevar <- gsub(" ", "_", nameold)
  setnames(test, nameold, namevar)
  meanc = as.numeric(colMeans(test[,1,with=F], na.rm=T))
  plt <- ggplot(data = test, aes_string(x=colnames(test)[1])) +
    geom_histogram(binwidth = x) +
    geom_vline(data=test,aes_string(xintercept=as.numeric(meanc)),
               linetype="dashed", colour="red", size=1) + 
    ggtitle(paste0(as.character(npl), ". ",  
                   "# Obs: ", as.character(nrow(na.omit(test))),", ", 
                   "Mean: ", as.character(round(meanc, digits=2)))) +
    theme(plot.title=element_text(face="bold", size = 8))
  rm(test)
  return(plt)
}

# For meteorological variables - group by DATE, ZIPCODE
quick_hist_meteo <- function(y, x = 0) {
  npl <- y-13
  test=data1[,c(1,2,y), with=F]
  test <- na.omit(test)
  x = ifelse(x==0, (max(test[,3,with=F])-min(test[,3,with=F]))/30, x)
  nameold <- colnames(test)[3]
  namevar <- gsub(" ", "_", colnames(test)[3])
  setnames(test, nameold, "temp")
  test <- ddply(test, .(DATE, ZIPCODE), summarise,
                temp=mean(temp, na.rm=T))
  setnames(test, "temp", namevar)
  meanc = mean(test[,3], na.rm=T)
  plt <- ggplot(data = test, aes_string(x=colnames(test)[3])) +
    geom_histogram(binwidth = x) +
    geom_vline(data=test,aes_string(xintercept=as.numeric(meanc)),
               linetype="dashed", colour="red", size=1) + 
    ggtitle(paste0(as.character(npl), ". ",  
                   "# Obs: ", as.character(nrow(na.omit(test))),", ", 
                   "Mean: ", as.character(round(meanc, digits=2)))) +
    theme(plot.title=element_text(face="bold", size = 8))
  rm(test)
  return(plt)
}

# For demographic variables - group by DATE, ZIPCODE
quick_hist_demos <- function(y, x = 0) {
  npl <- y-13
  test=data1[,c(1,y), with=F]
  test <- na.omit(test)
  x = ifelse(x==0, (max(test[,2,with=F])-min(test[,2,with=F]))/30, x)
  nameold <- colnames(test)[2]
  namevar <- gsub(" ", "_", colnames(test)[2])
  setnames(test, nameold, "temp")
  test <- ddply(test, .(ZIPCODE), summarise,
                temp=mean(temp, na.rm=T))
  colnames(test)[2] <- namevar
  meanc = mean(test[,2], na.rm=T)
  plt <- ggplot(data = test, aes_string(x=colnames(test)[2])) +
    geom_histogram(binwidth = x) +
    geom_vline(data=test,aes_string(xintercept=as.numeric(meanc)),
               linetype="dashed", colour="red", size=1) + 
    ggtitle(paste0(as.character(npl), ". ",  
                   "# Obs: ", as.character(nrow(na.omit(test))),", ", 
                   "Mean: ", as.character(round(meanc, digits=2)))) +
    theme(plot.title=element_text(face="bold", size = 8))
  rm(test)
  return(plt)
}


# Plot liquor sales stuff
multiplot(quick_hist(14), quick_hist(15), quick_hist(16),
          quick_hist(17), quick_hist(18), quick_hist(19), cols=2)

# Adjust them so we see more things
multiplot(quick_hist(14, 2) + coord_cartesian(xlim=c(0, 50)), 
          quick_hist(15, 50) + coord_cartesian(xlim=c(0, 2000)), 
          quick_hist(16,0.1) + scale_x_log10(), quick_hist(17, 0.1) + scale_x_log10(), 
          quick_hist(18,2) + coord_cartesian(c(0,100)),
          quick_hist(19, 0.1) + scale_x_log10(), cols=3)

# Plot meteorological variables
multiplot(quick_hist_meteo(22), 
          quick_hist_meteo(23),
          quick_hist_meteo(24),
          quick_hist_meteo(25),
          cols = 2)

# Adjust meteorological variables
multiplot(quick_hist_meteo(22, 0.1) + scale_x_log10(), 
          quick_hist_meteo(23, 0.1) + scale_x_log10(),
          quick_hist_meteo(24, 2),
          quick_hist_meteo(25, 2),
          cols = 2)

# Plot demographic variables
multiplot(quick_hist_demos(20),
          quick_hist_demos(26), quick_hist_demos(27), quick_hist_demos(28),
          quick_hist_demos(29), quick_hist_demos(30), quick_hist_demos(31),
          quick_hist_demos(32), quick_hist_demos(33), quick_hist_demos(34),
          quick_hist_demos(35), quick_hist_demos(36), quick_hist_demos(39),
          quick_hist_demos(40), quick_hist_demos(41), quick_hist_demos(42), 
          quick_hist_demos(43), quick_hist_demos(44), quick_hist_demos(45),
          cols = 5)


ggplot(data=gr_cat, aes(x=`CATEGORY NAME`, y=bottles)) + geom_point()+geom_bar()
ggplot(data=gr_item, aes(x=DESCRIPTION, y=bottles)) + geom_point()

arrange(gr_cat, -bottles)
arrange(gr_item, -bottles)[1:3,]


multiplot(quick_hist(14) + theme(plot.title = element_text(size=6),
                                 axis.title = element_text(size=6)),
          quick_hist(15) + theme(plot.title = element_text(size=6),
                                 axis.title = element_text(size=6)),
          quick_hist(16) + theme(plot.title = element_text(size=6),
                                 axis.title = element_text(size=6)),
          quick_hist(17) + theme(plot.title = element_text(size=6),
                                 axis.title = element_text(size=6)),
          quick_hist(18) + theme(plot.title = element_text(size=6),
                                 axis.title = element_text(size=6)),
          quick_hist(19) + theme(plot.title = element_text(size=6),
                                 axis.title = element_text(size=6)),
          cols=2)

test <- data1[, c(21,31), with=F]
ggpairs(gr_zip.only_pop, columns=4:23, axisLabels = 'internal',
        upper = "blank", lower=list(params=list(size=1)),
        params = c(Shape = I("."), outlier.shape = I("."))) + 
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

pairs1 <- data1[data1$ZIPCODE==50314][,c(14, 15,16,17,18,19),with=F]
setnames(pairs, "STATE BTL COST", "STATE_BTL_COST")
setnames(pairs, "LITER SIZE", "LITER_SIZE")
setnames(pairs, "BTL PRICE", "BTL_PRICE")
setnames(pairs, "BOTTLE QTY", "BOTTLE_QTY")
ggpairs(pairs, axisLabels = 'internal',
        upper = "blank", lower=list(params=list(size=1)),
        params = c(Shape = I("."), outlier.shape = I("."))) + 
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())


pairs2 <- ddply(data1,
                .(ZIPCODE), summarise, BOTTLES = sum(`BOTTLE QTY`, na.rm=T), 
                POPULATION = mean(POPULATION, na.rm=T), TOTAL=sum(TOTAL, na.rm=T),
                TOTAL_EST=sum(TOTAL_EST, na.rm=T), PRICE=mean(`BTL PRICE`, na.rm=T))
pairs2 <- mutate(pairs2, bottles_norm=BOTTLES/POPULATION, total_norm=TOTAL/POPULATION, 
                 total_est_norm=TOTAL_EST/POPULATION)

drops <- c("BOTTLES", "TOTAL", "POPULATION", "TOTAL_EST")
pairs2 <- pairs2[,!(names(pairs2) %in% drops)]

temp <- ddply(data1[,c(1,26:45), with=F], .(ZIPCODE), numcolwise(mean, na.rm=T))
pairs2 <- left_join(pairs2,temp)
rm(temp)

ggpairs(pairs2, columns=c(2,3,4,5,11,12,13), axisLabels = 'internal',
        upper = "blank", lower=list(params=list(size=1)),
        params = c(Shape = I("."), outlier.shape = I("."))) + 
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

pairs3 <- ddply(data1,
                .(CATEGORY), summarise, BOTTLES = sum(`BOTTLE QTY`, na.rm=T), 
                POPULATION = mean(POPULATION, na.rm=T), TOTAL=sum(TOTAL, na.rm=T),
                TOTAL_EST=sum(TOTAL_EST, na.rm=T), PRICE=mean(`BTL PRICE`, na.rm=T))

temp <- ddply(data1[,c(9,26:45), with=F], .(CATEGORY), numcolwise(mean, na.rm=T))
pairs3 <- left_join(pairs3, temp)

ggpairs(pairs3, columns=c(2,8,9,12,13,14,24), axisLabels = 'internal',
        upper = "blank", lower=list(params=list(size=1)),
        params = c(Shape = I("."), outlier.shape = I("."))) + 
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

colnames(gr_date_cat.spread)

ggplot(data=gr_date_cat.spread[,c(1,64,65)]) + geom_line(aes(x=DATE, y=TEQUILA)) + geom_line(aes(x=DATE, y=`TRIPLE SEC`), colour="red")




test1 <- ggplot(data=gr_date_cat.spread ,aes(x=`HIGH PROOF BEER`, y=TEQUILA)) + geom_line()+
  geom_smooth(method=lm, colour="red") +  
  geom_text(x = 500, y = 10000, label = lm_eqn(lm(TEQUILA~`HIGH PROOF BEER`,gr_date_cat.spread)), parse = TRUE)
test2 <- ggplot(data=gr_date_cat.spread ,aes(x=`PEACH BRANDIES`, y=TEQUILA)) + geom_line()+
  geom_smooth(method=lm, colour="red") +  
  geom_text(x = 500, y = 10000, label = lm_eqn(lm(TEQUILA~`PEACH BRANDIES`,gr_date_cat.spread)), parse = TRUE)
test3 <- ggplot(data=gr_date_cat.spread ,aes(x=`DECANTERS & SPECIALTY PACKAGES`, y=TEQUILA)) + geom_line()+
  geom_smooth(method=lm, colour="red") +  
  geom_text(x = 500, y = 10000, label = lm_eqn(lm(TEQUILA~`DECANTERS & SPECIALTY PACKAGES`,gr_date_cat.spread)), parse = TRUE)



multiplot()

lm_eqn = function(m) {
  
  l <- list(a = format(coef(m)[1], digits = 2),
            b = format(abs(coef(m)[2]), digits = 2),
            r2 = format(summary(m)$r.squared, digits = 3));
  
  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)    
  }
  
  as.character(as.expression(eq));                 
}



test1 <- ggplot(data=gr_date_cat.spread[,c(64,44)],aes(x=`MISC. IMPORTED CORDIALS & LIQUEURS`, y=TEQUILA)) +
  geom_line() +
  geom_smooth(method=lm) +  
  geom_text(x = 500, y = 10000, 
            label = lm_eqn(lm(TEQUILA~`MISC. IMPORTED CORDIALS & LIQUEURS`,gr_date_cat.spread[c(64,44)])),
            parse = TRUE, size = 3)

test2 <- ggplot(data=gr_date_cat.spread[,c(64,65)],aes(x=`TRIPLE SEC`, y=TEQUILA)) +
  geom_line() +
  geom_smooth(method=lm) +  
  geom_text(x = 500, y = 10000, 
                label = lm_eqn(lm(TEQUILA~`TRIPLE SEC`,gr_date_cat.spread[c(64,65)])),
                parse = TRUE, size = 3)

test3 <- ggplot(data=gr_date_cat.spread[,c(64,39)],aes(x=`IMPORTED VODKA - MISC`, y=TEQUILA)) +
  geom_line() +
  geom_smooth(method=lm) +  
  geom_text(x = 500, y = 10000, 
            label = lm_eqn(lm(TEQUILA~`IMPORTED VODKA - MISC`,gr_date_cat.spread[c(64,39)])),
            parse = TRUE, size = 3)


test4 <- ggplot(data=gr_date_cat.spread[,c(64,33)],aes(x=`HIGH PROOF BEER`, y=TEQUILA)) +
  geom_line() +
  geom_smooth(method=lm, colour="red") +  
  geom_text(x = 5, y = 15000, 
            label = lm_eqn(lm(TEQUILA~`HIGH PROOF BEER`,gr_date_cat.spread[c(64,33)])),
            parse = TRUE, size = 3)

test5 <- ggplot(data=gr_date_cat.spread[,c(64,48)],aes(x=`PEACH BRANDIES`, y=TEQUILA)) +
  geom_line() +
  geom_smooth(method=lm, colour="red") +  
  geom_text(x = 500, y = 10000, 
            label = lm_eqn(lm(TEQUILA~`PEACH BRANDIES`,gr_date_cat.spread[c(64,48)])),
            parse = TRUE, size = 3)

test6 <- ggplot(data=gr_date_cat.spread[,c(64,26)],aes(x=`DECANTERS & SPECIALTY PACKAGES`, y=TEQUILA)) +
  geom_line() +
  geom_smooth(method=lm, colour="red") +  
  geom_text(x = 5000, y = 12000, 
            label = lm_eqn(lm(TEQUILA~`DECANTERS & SPECIALTY PACKAGES`,gr_date_cat.spread[c(64,26)])),
            parse = TRUE, size = 3)

multiplot(test1, test2, test3, test4, test5, test6, cols=2)



  



# Redefine gr_date
dec_dates <- decimal_date(as.Date(c("2014-01-01", "2014-02-01", "2014-03-01",
                                    "2014-04-01", "2014-05-01", "2014-06-01",
                                    "2014-07-01", "2014-08-01", "2014-09-01",
                                    "2014-10-01", "2014-11-01", "2014-12-01")))

gr_date_cat2.month <- mutate(gr_date_cat2.month, 
                             month_nom =ifelse(Month=="January", dec_dates[1], 
                                                                   ifelse(Month=="February", dec_dates[2],
                                                                          ifelse(Month=="March", dec_dates[3],
                                                                                 ifelse(Month=="April", dec_dates[4],
                                                                                        ifelse(Month=="May", dec_dates[5],
                                                                                               ifelse(Month=="June", dec_dates[6],
                                                                                                      ifelse(Month=="July", dec_dates[7],
                                                                                                             ifelse(Month=="August", dec_dates[8],
                                                                                                                    ifelse(Month=="September", dec_dates[9],
                                                                                                                           ifelse(Month=="October", dec_dates[10],
                                                                                                                                  ifelse(Month=="November", dec_dates[11],
                                                                                                                                         ifelse(Month=="December", dec_dates[12], 0)))))))))))))


txmin <- decimal_date(as.Date(c("2014-01-01", "2014-03-01", "2014-06-01","2014-09-01", "2014-12-01")))
txmax <- c(decimal_date(as.Date(c("2014-03-01", "2014-06-01", "2014-09-01","2014-12-01"))), Inf)
tymin <- rep(-Inf, 5)
tymax <- rep(Inf, 5)
tfill= c("1.winter", "2.spring","3.summer","4.autumn","1.winter")
colScale <- c(col2hcl("blue"), col2hcl("green"), col2hcl("red"), col2hcl("purple"), col2hcl("blue"))
rects <- data.frame(txmin,txmax,tymin,tymax,tfill)
rects[,5] <- sapply(rects[,5], as.character)

ggplot(data=gr_date_cat2.month) +
  geom_rect(data=rects, aes(xmin=txmin,xmax=txmax,ymin=tymin,ymax=tymax, 
                            fill=tfill), alpha=0.4)+
  scale_fill_manual(values=colScale)+
  geom_line(aes(x=month_nom, y=BCAT_NORM, group=1))+
  facet_wrap(~CATEGORY, scales="free_y")+
  labs(x="Month", y = "Normalised bottle sales", 
       title = "Liquor categories bottle sales with season overlay") + 
  theme(panel.background = element_rect(fill = 'white'),
        strip.text = element_text(size=7),
        strip.background = element_rect(fill="white"),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_text(size = rel(1), colour = "blue"))

# Correlation table
require(lattice)
require(gdata)
z <- as.matrix(corr.cat.ts)
upperTriangle(z, diag=T) <- ""
levelplot(z,scales=list(x=list(rot=90, abbreviate=TRUE, minlength=8, tck=0.3),
                        y=list(tck=0.3)),
          main="Correlation Matrix",col.regions= topo.colors, 
          cuts=4, at=seq(-1,1,0.4), xlab="", ylab="",
          colorkey=list(labels=list(at=seq(-0.8,0.8,0.4), 
                                    labels=c("High Inverse", "Med Inverse", 
                                             "I\snsignificant", "Med Positive",
                                             "High Positive"))))

ggplot(melt(z), aes(X1, X2, fill = value)) + geom_tile() + 
  scale_fill_gradient(low = "blue",  high = "yellow")

#Useful: make palette
library(colorspace)
custom <- choose_palette()
test <- NULL
ptest <- corr.cat.ts
ptest[] <- NA
for (i in 1:69){
  test <- as.vector(apply(corr.cat.ts[i,], 1, function(x) order(x, decreasing=T)[2:4]))
  ptest[i,test] <- corr.cat.ts[i,test]
  test <- as.vector(apply(corr.cat.ts[i,], 1, function(x) order(x, decreasing=F)[2:4]))
  ptest[i,test] <- corr.cat.ts[i,test]
}

# Create new data frame based on gr_date_cat2
gr_date_cat2.month_v3 <- gr_date_cat2.month
gr_date_cat2.month_v3 <- mutate(gr_date_cat2.month_v3, 
       season=ifelse(Month %in% c("January", "February", "December"), "2.winter", 
                     ifelse(Month %in% c("March", "April", "May"), "3.spring",
                            ifelse(Month %in% c("June", "July", "August"), "1.summer",
                                   "4.autumn"))))

gr_date_cat2.month_v3 <- ddply(gr_date_cat2.month_v3, .(CATEGORY,season), summarise, 
      BOTTLES=sum(BOTTLES, na.rm=T))

gr_date_cat2.month_v3.temp <- ddply(gr_date_cat2.month_v3, .(CATEGORY), summarise, 
                               BOTTLES_SUM=sum(BOTTLES, na.rm=T))

gr_date_cat2.month_v3 <- left_join(gr_date_cat2.month_v3,
                                   gr_date_cat2.month_v3.temp, by="CATEGORY")
rm(gr_date_cat2.month_v3.temp)


# At this stage our df
gr_date_cat2.month_v3 <- mutate(gr_date_cat2.month_v3, 
                                BOTTLES_PROP=BOTTLES/BOTTLES_SUM)

#gr_date_cat2.month_v2 <- mutate(gr_date_cat2.month_v2, 
#                                season_type=ifelse(season %in% c("3.spring", "1.summer"),
#                                                   "warm", "cold"))

#ggplot(data=gr_date_cat2.month_v2, aes(x=CATEGORY, y=BOTTLES_PROP,
#                                       fill=season)) + 
#  geom_bar(stat="identity", position = "stack")

#ggplot(data=gr_date_cat2.month_v2, aes(x=1, y=BOTTLES_PROP, fill=season)) + 
#  geom_bar(stat="identity",width=1) +
#  facet_wrap(~CATEGORY)+coord_polar(theta="y") +
#  scale_fill_brewer(palette = "Set1") +
#  theme(axis.text = element_blank(),
#        axis.ticks = element_blank(),
#        panel.grid  = element_blank(),
#        panel.background = element_blank(),
#        strip.background = element_rect(fill="white"))


#ggplot(data=gr_date_cat2.month_v2, aes(x=1, y=BOTTLES_PROP, fill=season_type)) + 
#  geom_bar(stat="identity", position=position_dodge(width=1)) +
#  facet_wrap(~CATEGORY)


#Categorising to broader categories
categories <- unique(gr_date_cat2.month_v3[,1])
Vodkas <- categories[c(1,2,28,37,36,40,45)]
Misc <-  categories[c(3,5,24,25,31,51, 65)]
Liqueurs <-  categories[c(4,9,20,21,22,23,30,32,41,42,63,67,68,69)]
Gins <-  categories[c(6,8,26,33)]
Brandies <-  categories[c(7,11,13,18, 34,43,46)]
Whiskies <-  categories[c(12,14,15,17,38,53,54,55,58,59,61)]
Schnapps <-  categories[c(10,16,19,29,35,44,47,48,50,52,56,60,64,66)]
Rums <- categories[c(27,39,49,57)]
Tequila <- categories[62]

gr_date_cat2.month_v3 <- mutate(gr_date_cat2.month_v3, 
                                broad_cat=ifelse(CATEGORY %in% Vodkas, "Vodkas",
                                                 ifelse(CATEGORY %in% Misc, "Misc",
                                                        ifelse(CATEGORY %in% Liqueurs, "Liqueurs",
                                                               ifelse(CATEGORY %in% Gins, "Gins",
                                                                      ifelse(CATEGORY %in% Brandies, "Brandies",
                                                                             ifelse(CATEGORY %in% Whiskies, "Whiskies",
                                                                                    ifelse(CATEGORY %in% Schnapps, "Schnapps", 
                                                                                           ifelse(CATEGORY %in% Rums, "Rums", "Tequila")))))))))


gr_date_cat2.month_v4 <- ddply(gr_date_cat2.month_v3, .(broad_cat, season), summarise, 
                               BOTTLES=sum(BOTTLES, na.rm=T))

ggplot(data=gr_date_cat2.month_v4, aes(x=season, y=BOTTLES, group=broad_cat, colour=broad_cat)) +
  geom_line()  +  scale_x_discrete(limits=c("2.winter", "3.spring", "1.summer", "4.autumn"))

gr_date_cat2.month_v4.temp <- ddply(gr_date_cat2.month_v4, .(broad_cat), summarise, 
                                    BOTTLES_SUM=sum(BOTTLES, na.rm=T))

gr_date_cat2.month_v4 <- left_join(gr_date_cat2.month_v4,
                                   gr_date_cat2.month_v4.temp, by="broad_cat")

rm(gr_date_cat2.month_v4.temp)


gr_date_cat2.month_v4.temp <- ddply(gr_date_cat2.month_v4, .(season), summarise, 
                                    BOTTLES_SUM=sum(BOTTLES, na.rm=T))

gr_date_cat2.month_v4 <- mutate(gr_date_cat2.month_v4, 
                                BOTTLES_PROP=BOTTLES/BOTTLES_SUM)

gr_date_cat2.month_v4.temp$BOTTLES_PROP_ADJ <- gr_date_cat2.month_v4.temp$BOTTLES_SUM/sum(gr_date_cat2.month_v4.temp$BOTTLES_SUM)

gr_date_cat2.month_v4 <- left_join(gr_date_cat2.month_v4,
                                   gr_date_cat2.month_v4.temp, by="season")

gr_date_cat2.month_v4$BOTTLES_ADJ <- gr_date_cat2.month_v4$BOTTLES_PROP/gr_date_cat2.month_v4$BOTTLES_PROP_ADJ

gr_date_cat2.month_v4 <- rename(gr_date_cat2.month_v4, Type=broad_cat)

test <- mutate(gr_date_cat2.month_v4, 
               Seasonal_Profile=ifelse(Type %in% c("Vodkas", "Tequila", "Gins", "Rums"),
                              "Warm", "Cold"))
ggplot(data=gr_date_cat2.month_v4, aes(x=season, y=BOTTLES_ADJ,  group=Type, colour=Type)) +
  geom_line(linetype=2, size=1) +  geom_point(size=3) + scale_x_discrete(limits=c("2.winter", "3.spring", "1.summer", "4.autumn"))+
  facet_wrap(~Type, scales="free_y") + labs(x="Season", y="Seasonally adjusted bottle sales ratio")

colScale <- c(col2hcl("blue"), col2hcl("green"), col2hcl("red"), col2hcl("purple"), col2hcl("blue"))
colScale2 <- c(col2hcl("blue"), col2hcl("red"))
ggplot(data=gr_date_cat2.month_v4, aes(x=season, y=BOTTLES_ADJ,  group=Type, colour=Seasonal_Profile)) +
  geom_rect(data=rects, aes(xmin=txmin,xmax=txmax,ymin=tymin,ymax=tymax, 
                            fill=season), alpha=0.4)+
  scale_fill_manual(values=colScale)+
  geom_line(linetype=2, size=1) +  geom_point(size=3) + scale_x_discrete(limits=c("2.winter", "3.spring", "1.summer", "4.autumn"))+
  facet_wrap(~Type, scales="free_y") + labs(x="Season", y="Seasonally adjusted bottle sales ratio")+
  scale_color_manual(values=colScale2)

