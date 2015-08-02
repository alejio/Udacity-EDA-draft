#Total spent obviously correlated with bottles. Let's prove it.
qplot(data=data1, x=`BOTTLE QTY`, y=TOTAL)


gr_date_cat.spread <- spread(gr_date_cat[,1:3], `CATEGORY NAME`, sum_BOTTLE_QTY)
#Plotting tequila vs triple sec; 
#Typical ratio is 2 parts tequila and 1 part triple sec. Drawing corresponding abline
ggplot(data=gr_date_cat.spread[,64:65], aes(y=TEQUILA, x=`TRIPLE SEC`)) + geom_point() + geom_abline(slope=2)
ggplot(data=gr_date_cat.spread[,c(57,64)], aes(y=TEQUILA, x=`SINGLE MALT SCOTCH`)) + geom_point()
#Create correlations data frame.
category_corrs <- data.table(rcorr(as.matrix(gr_date_cat.spread[,2:70]))$r)
category_corrs <- as.data.table(sapply(category_corrs, function(x) if("numeric" %in% class(x) ) { 
  floor(100*x)/100
} ))
category_corrs.samples <- data.table(rcorr(as.matrix(gr_date_cat.spread[,2:70]))$n)
category_corrs.signif <- data.table(rcorr(as.matrix(gr_date_cat.spread[,2:70]))$P)
category_corrs.signif2 <- sapply(category_corrs.signif, function(x)
  ifelse(x < .001, "red4", ifelse(x < .01, "red" , ifelse(x < .05, "orange", "royalblue"))))
category_corrs$`CATEGORY NAME` <- gr_cat$`CATEGORY NAME`
#can also use function corstarsl which pretty prints this
#Show top correlated categories for tequila
sort(category_corrs["TEQUILA",], decreasing = T)[2:6]
#As expected triple sec is there!


#Plot correlations
#Make x and y vectors
cat_num.x <- c()
corrs_cat.y <- c()
corrs_cat.samples.size <- c()
for (i in 1:69){
  cat_num.x <- append(cat_num.x, rep(gr_cat$`CATEGORY NAME`[i], 69), after = 69*(i-1))
  corrs_cat.y <- append(corrs_cat.y, category_corrs[,i,with=F])
  corrs_cat.samples.size <- append(corrs_cat.samples.size, category_corrs.samples[,i,with=F])
}
#use unlist to proper merge list of lists
corrs_cat.y <- unlist(corrs_cat.y)
corrs_cat.samples.size <- unlist(corrs_cat.samples.size)/max(corrs_cat.samples.size, na.rm=T)

#Plot
ggplot(data=category_corrs ,aes(x=cat_num.x, y=corrs_cat.y)) + geom_point() + 
  scale_x_discrete(labels=abbreviate) + theme(axis.text.x  = element_text(angle=90, size=6))


#Repeat process by removing duplicate correlations
category_corrs.tri <- category_corrs
category_corrs.tri[upper.tri(category_corrs.tri, diag=T)] <- ""
category_corrs.samples.tri <- category_corrs.samples
category_corrs.samples.tri[upper.tri(category_corrs.samples.tri, diag=T)] <- ""
category_corrs.signif2.tri <- category_corrs.signif2
category_corrs.signif2.tri[upper.tri(category_corrs.signif2.tri, diag=T)] <- ""

corrs_cat.y <- c()
corrs_cat.samples.size <- c()
corrs_cat.signif2.colour <- c()
for (i in 1:69){
  corrs_cat.y <- append(corrs_cat.y, category_corrs.tri[,i,with=F])
  corrs_cat.samples.size <- append(corrs_cat.samples.size, category_corrs.samples.tri[,i,with=F])
  corrs_cat.signif2.colour <- append(corrs_cat.signif2.colour, category_corrs.signif2.tri[,i])
}
#use unlist to proper merge list of lists
corrs_cat.y <- as.numeric(unlist(corrs_cat.y))
corrs_cat.samples.size <- as.numeric(unlist(corrs_cat.samples.size))
corrs_cat.signif2.colour <- unlist(corrs_cat.signif2.colour)

library(stringr)
ggplot(data=category_corrs.tri ,aes(x=cat_num.x, y=corrs_cat.y)) +
  geom_point(size=2.5*corrs_cat.samples.size/max(corrs_cat.samples.size, na.rm=T),
             colour=corrs_cat.signif2.colour, alpha=0.7) + 
  theme(axis.text=element_text(angle=90,size=8)) + scale_x_discrete(labels = abbreviate)

scale_x_discrete(labels = function(x) str_wrap(x, width = 1))