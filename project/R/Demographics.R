
# Explore demographics ----------------------------------------------------

#Let's get population data by zipcode.
gr_zip_pop <- ddply(data1[,c(1,24, seq(29,68,2), 72, 73), with=F], .(ZIPCODE), 
                numcolwise(mean, na.rm=T))
gr_zip_pop$ZIPCODE <- factor(gr_zip$ZIPCODE, ordered=F)


#Get map of Iowa using RgoogleMaps and ggmap
CenterOfMap <- geocode("41.9137948,-93.3293731")
Iowa <- get_map(c(lon=CenterOfMap$lon, lat=CenterOfMap$lat), zoom = 7,
                   source = "stamen", maptype= "toner", color="bw")
#use geocodeQueryCheck() to check how many I've got left
# Plot the map with population and university overlays
IowaMap <- ggmap(Iowa)
IowaMap + 
  geom_point(data=gr_zip_pop, aes(x=STORE_LON, y=STORE_LAT, size=POPULATION),
             colour="red", alpha=0.5) +
  geom_point(data = unis, aes(x=lon, y=lat, size=enrolled), colour="blue",
             shape=17, alpha=0.5) + 
  theme(legend.position="none", axis.title.x=element_blank(),
        axis.title.y=element_blank(), axis.ticks=element_blank(),
        axis.text.x=element_blank(), axis.text.y=element_blank(),
        plot.title = element_text(size = rel(1.5), colour = "blue")) +
  labs(title="Map of Iowa with population and university overlays")

#Get populations by city


gr_city_pop <- ddply(data1, .(ZIPCODE, CITY), summarise, 
                     POPULATION = mean(POPULATION, na.rm=T), 
                     STORE_LAT = mean(STORE_LAT, na.rm = T),
                     STORE_LON = mean(STORE_LON, na.rm = T))
gr_city_pop <- ddply(gr_city_pop, .(CITY), summarise, 
                     TOT_POP = sum(POPULATION, na.rm = T),
                     LAT = mean(STORE_LAT, na.rm = T), 
                     LON = mean(STORE_LON, na.rm =T))

IowaMap + 
  geom_point(data=gr_city_pop, aes(x=LON, y=LAT, size=TOT_POP),
             colour="purple", alpha=1) +
  theme(legend.position="none", axis.title.x=element_blank(),
        axis.title.y=element_blank(), axis.ticks=element_blank(),
        axis.text.x=element_blank(), axis.text.y=element_blank(),
        plot.title = element_text(size = rel(1.5), colour = "blue")) +
  labs(title="Map of Iowa with city population overlay")

gr_county_pop <- ddply(data1, .(ZIPCODE, COUNTY), summarise, 
                     POPULATION = mean(POPULATION, na.rm=T), 
                     STORE_LAT = mean(STORE_LAT, na.rm = T),
                     STORE_LON = mean(STORE_LON, na.rm = T))
gr_county_pop <- ddply(gr_county_pop, .(COUNTY), summarise, 
                     TOT_POP = sum(POPULATION, na.rm = T),
                     LAT = mean(STORE_LAT, na.rm = T), 
                     LON = mean(STORE_LON, na.rm =T))

IowaMap + 
  geom_point(data=gr_county_pop, aes(x=LON, y=LAT, size=TOT_POP),
             colour="purple", alpha=1) +
  theme(legend.position="none", axis.title.x=element_blank(),
        axis.title.y=element_blank(), axis.ticks=element_blank(),
        axis.text.x=element_blank(), axis.text.y=element_blank(),
        plot.title = element_text(size = rel(1.5), colour = "blue")) +
  labs(title="Map of Iowa with county population overlay")

#Produce maps with overlays for all nationalities
#Quick-call function for plotting
plot_map_nat_over <- function(y){
  x <- colnames(gr_zip_pop)[y]
  IowaMap + 
    geom_point(data=gr_zip_pop, aes_string(x="STORE_LON", y="STORE_LAT",
                                           colour=x, alpha=x)) +
    scale_colour_gradientn(colours = topo.colors(10), limits=c(0,50)) +
    theme(legend.position="none", axis.title.x=element_blank(),
          axis.title.y=element_blank(), axis.ticks=element_blank(),
          axis.text.x=element_blank(), axis.text.y=element_blank(),
          plot.title = element_text(size = rel(1), colour = "blue")) +
    labs(title=x)
}


multiplot(plot_map_nat_over(3), plot_map_nat_over(6), plot_map_nat_over(7), 
          plot_map_nat_over(8), plot_map_nat_over(9), plot_map_nat_over(10),
          plot_map_nat_over(11), plot_map_nat_over(12), plot_map_nat_over(13),
          plot_map_nat_over(14), plot_map_nat_over(15), plot_map_nat_over(16),
          plot_map_nat_over(17), plot_map_nat_over(18), plot_map_nat_over(19),
          plot_map_nat_over(20), plot_map_nat_over(21),
          cols=5)

#Add establishments variable
gr_zip_est <- ddply(data1[,c(1,23), with=F], .(ZIPCODE), 
                    TOTAL_EST = mean(TOTAL_EST, na.rm = T))
gr_zip_pop$TOTAL_EST <- gr_zip_est$TOTAL_EST
test <- unique(cbind(data1$ZIPCODE, data1$TOTAL_EST))
test <- data.table(test)
setnames(test, c(1,2), c("ZIPCODE", "TOTAL_EST"))
test$ZIPCODE <- factor(test$ZIPCODE)
gr_zip_pop <- left_join(gr_zip_pop, test)
rm(test)
#Plot
IowaMap + 
  geom_point(data=gr_zip_pop, aes(x=STORE_LON, y=STORE_LAT, size=TOTAL_EST),
             colour="red", alpha=0.5) +
  theme(legend.position="none", axis.title.x=element_blank(),
        axis.title.y=element_blank(), axis.ticks=element_blank(),
        axis.text.x=element_blank(), axis.text.y=element_blank(),
        plot.title = element_text(size = rel(1.5), colour = "blue")) +
  labs(title="Map of Iowa with establishment overlays")

