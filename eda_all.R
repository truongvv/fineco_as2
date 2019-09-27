install.packages("hrbrthemes")
library(hrbrthemes)

#John EDA
Combi_df <- data.frame(date=index(Combi), coredata(Combi))
Combi_df$date <- as.Date(Combi_df$date)
#Plotting ASX 200
ggplot(Combi_df, aes(date, asx)) + geom_line() +
  xlab("Date") + ylab("ASX Index") + ggtitle("Value of ASX 200 Index Over time") +
  annotate(geom="text", x=as.Date("2009-01-01"), y=7000,
           label="Market Peak before 2008 GFC (Sep 2007)") +
  annotate(geom="text", x=as.Date("2010-01-01"), y=3200,
           label="Market Trough (Jan 2009)") +
  coord_cartesian(clip = 'off') +
  annotate(geom="point", x=as.Date("2007-09-30"), y=6754, size=8, shape=21, fill="transparent") +
  annotate(geom="point", x=as.Date("2009-01-31"), y=3400, size=8, shape=21, fill="transparent") +
  geom_smooth(method='lm') +
  theme_ipsum()
#Plotting Oil Prices
ggplot(Combi_df, aes(date, oil)) + geom_line() +
  xlab("Date") + ylab("Oil Price Per Barrel") + ggtitle("Price of Oil Over time") +
  annotate(geom="text", x=as.Date("2008-01-01"), y=35,
           label="Oil Price Collapse due to GFC") +
  annotate(geom="text", x=as.Date("2017-01-01"), y=40,
           label="Price Collapse due to Increased Supply") +
  annotate(geom="text", x=as.Date("2014-01-01"), y=140,
           label="Price Spike due to Geopolitical Factors + Peak Oil Worries") +
  coord_cartesian(clip = 'off') +
  annotate(geom="point", x=as.Date("2009-01-31"), y=40, size=8, shape=21, fill="transparent") +
  annotate(geom="point", x=as.Date("2015-01-31"), y=50, size=8, shape=21, fill="transparent") +
  annotate(geom="point", x=as.Date("2008-07-01"), y=133, size=8, shape=21, fill="transparent") +
  geom_smooth(method='lm') +
  theme_ipsum()
#Gold price
ggplot(Combi_df, aes(date, gold_price_london_fixing)) + geom_line() +
  xlab("Date") + ylab("Gold Price $") + ggtitle("Price of Gold Over time") +
  annotate(geom="text", x=as.Date("2008-01-01"), y=1000,
           label="ASX 200 Trough GFC") +
  annotate(geom="text", x=as.Date("2007-01-01"), y=550,
           label="Peak of Market prior to GFC") +
  annotate(geom="text", x=as.Date("2011-09-01"), y=1850,
           label="Spike Due to Low Interest Rates + Rise of Developing Economies") +
  coord_cartesian(clip = 'off') +
  annotate(geom="point", x=as.Date("2009-01-31"), y=930, size=8, shape=21, color ="orange", fill="transparent") +
  annotate(geom="point", x=as.Date("2007-07-31"), y=650, size=8, shape=21, color ="orange", fill="transparent") +
  annotate(geom="point", x=as.Date("2011-09-01"), y=1800, size=8, shape=21, color ="orange", fill="transparent") +
  geom_smooth(method='lm') +
  theme_ipsum()
#ASX/DJIA
ggplot(Combi_df, aes(date)) +
  geom_line(aes(y=djia, color = "djia")) +
  geom_line(aes(y=asx, color = "asx")) +
  xlab("Date") + ylab("ASX and DJIA Index's") + ggtitle("ASX 200 Vs DJIA") +
  annotate(geom="text", x=as.Date("2009-01-01"), y=7000,
           label="Market Peak before 2008 GFC (Sep 2007)") +
  annotate(geom="text", x=as.Date("2010-01-01"), y=3200,
           label="Market Trough (Jan 2009)") +
  annotate(geom="text", x=as.Date("2015-01-01"), y=10000,
           label="Market drops due to Chinese Market fluctuations") +
  annotate(geom="text", x=as.Date("2019-01-01"), y=28000,
           label="Tech Boom - FAANG") +
  coord_cartesian(clip = 'off') +
  annotate(geom="point", x=as.Date("2007-09-30"), y=6754, size=8, shape=21, fill="transparent") +
  annotate(geom="point", x=as.Date("2009-01-31"), y=3400, size=8, shape=21, fill="transparent") +
  annotate(geom="point", x=as.Date("2015-09-30"), y=16466, size=8, shape=21, fill="transparent") +
  annotate(geom="point", x=as.Date("2015-09-30"), y=5200, size=8, shape=21, fill="transparent") +
  annotate(geom="point", x=as.Date("2017-12-31"), y=26000, size=10, shape=21, fill="transparent") +
  theme_ipsum()