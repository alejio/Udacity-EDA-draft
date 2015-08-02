bks <- seq(min(data1$`BOTTLE QTY`, na.rm=T), max(data1$`BOTTLE QTY`, na.rm=T))
ggplot(data=data1, aes(x=`BOTTLE QTY`)) +
  geom_histogram(aes(y =..density..),
                 col="red",
                 fill="green",
                 alpha = .2, origin = -0.5) +
  labs(title="Histogram for Log Bottle Quantities") +
  labs(x="Bottles", y="Count") +  scale_x_log10(breaks=bks, labels=NULL)