

#Get map
Iowa <- get_map(c(lon=CenterOfMap$lon, lat=CenterOfMap$lat), zoom = 7,
                   source = "stamen", maptype= "toner", color="bw")
#use geocodeQueryCheck() to check how many I've got left
# Plot the map
IowaMap <- ggmap(Iowa)
#Plotting population bubbles, overlay store locations, overlay uni location.
#Nice one, because since bubbles represent population, when a bubble is filled by
#little red dots, it means that there are enough liquor stores in the area.
#Could be useful in seeing opportunities for opening a store?
#Also plotting uni positions with point size showing enrollment 
#Overlaying unis, zipcodes, stores
cols <- c("Zipcode Centre"="green", "Store"="red", "Uni"="blue")
IowaMap + 
  geom_point(data = gr_zip, aes(x = STORE_LON, y = STORE_LAT, size=POPULATION,
                                color="Zipcode Centre"), alpha=0.5)+
  geom_point(data = gr_store, aes(x = lon, y = lat, color="Store"), size=2, alpha=0.5) +
  geom_point(data=unis, aes(x=lon, y=lat, size=enrolled, color="Uni"), shape=7)+
  scale_size_continuous(range = c(1, 18), guide=F) +
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), plot.title = element_text(lineheight=.8, face="bold")) +
  xlab("") + ylab("")+
  scale_colour_manual(name="Colours",values=cols) + ggtitle("Map of Iowa with overlays")



# Look at price elasticity of Hawkeye Vodka -------------------------------

test <- filter(data1, `DESCRIPTION`=="Hawkeye Vodka")
qplot(test$`BTL PRICE`)
#Dead end. Bottle price is fixed.


# Idea: Most expensive drinks analysis ------------------------------------

qplot(data=data1, `BTL PRICE`/`BOTTLE QTY`)
#Look at outliers
data1$DESCRIPTION[sort(data1$`BTL PRICE`/data1$`BOTTLE QTY`, decreasing = T, index.return=T)$ix[1:10]]
