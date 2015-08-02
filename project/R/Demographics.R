##Exploratory on demographics and stuff
#Let's organise data by zipcode.
gr_zip <- ddply(data1, .(ZIPCODE), numcolwise(mean, na.rm=T))
gr_zip$ZIPCODE <- factor(gr_zip$ZIPCODE, ordered=F)


gr_zip.only_pop = gr_zip[,c(1,3,14,seq(19,57,2))]
summary(gr_zip$ZIPCODE)
ggplot(data=gr_zip, aes(x=as.integer(ZIPCODE), y=POPULATION)) +geom_point() +geom_line() + 
  scale_x_discrete(breaks=seq(0, length(gr_zip$ZIPCODE), 20), labels=gr_zip$ZIPCODE[seq(0, length(gr_zip$ZIPCODE), 20)])
ggplot(data=gr_zip, aes(x=ZIPCODE, y=Perc_white)) + geom_point()
ggplot(data=gr_zip, aes(x=ZIPCODE, y=Perc_black)) + geom_point()
ggplot(data=gr_zip, aes(x=ZIPCODE, y=Perc_greek)) + geom_point()
subset.col <- c(6,23)
subset.n
test <- ggpairs(gr_zip.only_pop[,subset.col], params=list(corSize=3))
test

lapply(gr_zip.only_pop, mean, na.rm=T)
