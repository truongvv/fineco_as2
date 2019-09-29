library(hrbrthemes)
#John EDA
Combi_df <- data.frame(date=index(Combi), coredata(Combi))
Combi_df$date <- as.Date(Combi_df$date)
#1. Plotting ASX 200
ggplot(Combi_df, aes(date, asx)) + geom_line() + 
    xlab("Date") + ylab("ASX Index") + ggtitle("Value of ASX 200 Index Over 
time") + 
    annotate(geom="text", x=as.Date("2009-01-01"), y=7000, 
             label="Market Peak before 2008 GFC (Sep 2007)") +
    annotate(geom="text", x=as.Date("2010-01-01"), y=3200, 
             label="Market Trough (Jan 2009)") +
    coord_cartesian(clip = 'off') +
    annotate(geom="point", x=as.Date("2007-09-30"), y=6754, size=8, 
             shape=21, fill="transparent") +
    annotate(geom="point", x=as.Date("2009-01-31"), y=3400, size=8, 
             shape=21, fill="transparent") +
    geom_smooth(method='lm') +
    theme_ipsum()
#Month on month % change - experiment
#asx_mom <- Combi_df
#asx_mom1 <- asx_mom %>%
#arrange(date, .by_group = TRUE) %>%
#mutate(pct_change = (asx/lead(asx) - 1) * 100)
#ggplot(asx_mom1, aes(date, pct_change)) + geom_line()
#2. Plotting Oil Prices
ggplot(Combi_df, aes(date, oil)) + geom_line() + 
    xlab("Date") + ylab("Oil Price Per Barrel") + ggtitle("Price of Oil Over 
time") +
    annotate(geom="text", x=as.Date("2008-01-01"), y=35, 
             label="Oil Price Collapse due to GFC") +
    annotate(geom="text", x=as.Date("2017-01-01"), y=40, 
             label="Price Collapse due to Increased Supply") +
    annotate(geom="text", x=as.Date("2014-01-01"), y=140, 
             label="Price Spike due to Geopolitical Factors + Peak Oil Worries") 
+
    coord_cartesian(clip = 'off') +
    annotate(geom="point", x=as.Date("2009-01-31"), y=40, size=8, 
             shape=21, fill="transparent") +
    annotate(geom="point", x=as.Date("2015-01-31"), y=50, size=8, 
             shape=21, fill="transparent") +
    annotate(geom="point", x=as.Date("2008-07-01"), y=133, size=8, 
             shape=21, fill="transparent") +
    geom_smooth(method='lm') +
    theme_ipsum()
#3. Plotting Gold price
ggplot(Combi_df, aes(date, gold_price_london_fixing)) + geom_line() + 
    xlab("Date") + ylab("Gold Price $") + ggtitle("Price of Gold Over time") +
    annotate(geom="text", x=as.Date("2008-01-01"), y=1000, 
             label="ASX 200 Trough GFC") +
    annotate(geom="text", x=as.Date("2007-01-01"), y=550, 
             label="Peak of ASX prior to GFC") +
    annotate(geom="text", x=as.Date("2011-09-01"), y=1850, 
             label="Spike Due to Low Interest Rates + Rise of Developing 
Economies") +
    coord_cartesian(clip = 'off') +
    annotate(geom="point", x=as.Date("2009-01-31"), y=930, size=8, 
             shape=21, color ="orange", fill="transparent") +
    annotate(geom="point", x=as.Date("2007-07-31"), y=650, size=8, 
             shape=21, color ="orange", fill="transparent") +
    annotate(geom="point", x=as.Date("2011-09-01"), y=1800, size=8, 
             shape=21, color ="orange", fill="transparent") +
    geom_smooth(method='lm') +
    theme_ipsum()
#4. Plotting ASX/DJIA
ggplot(Combi_df, aes(date)) + 
    geom_line(aes(y=djia, color = "djia")) +
    geom_line(aes(y=asx, color = "asx")) +
    xlab("Date") + ylab("ASX and DJIA Index's") + ggtitle("ASX 200 Vs DJIA") 
+ 
    annotate(geom="text", x=as.Date("2009-01-01"), y=7000, 
             label="Market Peak before 2008 GFC (Sep 2007)") +
    annotate(geom="text", x=as.Date("2010-01-01"), y=3200, 
             label="Market Trough (Jan 2009)") +
    annotate(geom="text", x=as.Date("2015-01-01"), y=10000, 
             label="Market drops due to Chinese Market fluctuations") +
    annotate(geom="text", x=as.Date("2019-01-01"), y=28000, 
             label="Tech Boom - FAANG") +
    coord_cartesian(clip = 'off') +
    annotate(geom="point", x=as.Date("2007-09-30"), y=6754, size=8, 
             shape=21, fill="transparent") +
    annotate(geom="point", x=as.Date("2009-01-31"), y=3400, size=8, 
             shape=21, fill="transparent") +
    annotate(geom="point", x=as.Date("2015-09-30"), y=16466, size=8, 
             shape=21, fill="transparent") +
    annotate(geom="point", x=as.Date("2015-09-30"), y=5200, size=8, 
             shape=21, fill="transparent") +
    annotate(geom="point", x=as.Date("2017-12-31"), y=26000, size=10, 
             shape=21, fill="transparent") +
    theme_ipsum()
#5. Plotting Imports/Exports
i_e <- ggplot(Combi_df, aes(date)) + 
    geom_line(aes(y=abs_imports, color = "abs_imports")) +
    geom_line(aes(y=abs_exports, color = "abs_exports")) +
    xlab("Date") + ylab("Value in $") + ggtitle("Australian Imports and 
Exports") + 
    theme_ipsum()  
#remove scientific notation
options(scipen=999)
i_e
#Divergence at the end - likely caused by high levels of household debt 
(post-property boom), stagnant wages
#Economy relies heavily on mining and construction - both are down
#6. Plotting Unemployment
ggplot(Combi_df, aes(date, unemployment)) + geom_line() + 
    xlab("Date") + ylab("Unemployment Rate %") + ggtitle("Unemployment 
%") +
    annotate(geom="text", x=as.Date("2010-01-01"), y=6.3, 
             label="Spike due to GFC") +
    annotate(geom="text", x=as.Date("2015-01-30"), y=7.2, 
             label="Spike due to end of mining boom") +
    coord_cartesian(clip = 'off') +
    annotate(geom="point", x=as.Date("2009-03-31"), y=6.1, size=8, 
             shape=21, fill="transparent") +
    annotate(geom="point", x=as.Date("2015-01-30"), y=7, size=8, 
             shape=21, fill="transparent") +
    geom_smooth(method='lm') +
    theme_ipsum()
#seasonal fluctuations caused by school leavers and post-christmas low 
demand for workers
#7. Plotting Unemployment, rba cash rate, inflation 
ggplot(Combi_df, aes(date)) + 
    geom_line(aes(y=unemployment, color = "unemployment")) +
    geom_line(aes(y=rba_cash_rate, color = "rba_cash_rate")) +
    geom_line(aes(y=yearly_inflation, color = "yearly_inflation")) +
    geom_line(aes(y=quarterly_inflation, color = "quarterly_inflation")) +
    xlab("Date") + ylab("%") + ggtitle("Unemployment, Cash Rate and 
Inflation %") +
    theme_ipsum()
#8. Plotting Iron
ggplot(Combi_df, aes(date, iron)) + geom_line() + 
    xlab("Date") + ylab("Iron Ore Price $") + ggtitle("Iron Ore Price") +
    annotate(geom="text", x=as.Date("2015-11-30"), y=30, 
             label="Price collapse due to end of mining boom") +
    coord_cartesian(clip = 'off') +
    annotate(geom="point", x=as.Date("2015-11-30"), y=42, size=8, 
             shape=21, fill="transparent") +
    geom_smooth(method='lm') +
    theme_ipsum()
#Interesting to note that price seems to be completely unaffected by the 
GFC
#9. Plotting OECD Leading Indicators
ggplot(Combi_df, aes(date, oecd_li)) + geom_line() + 
    xlab("Date") + ylab("OECD Leading Indicators") + ggtitle("OECD Leading 
Indicators") +
    coord_cartesian(clip = 'off') +
    annotate(geom="text", x=as.Date("2009-02-28"), y=97.3, 
             label="GFC") +
    coord_cartesian(clip = 'off') +
    annotate(geom="point", x=as.Date("2009-02-28"), y=97.53, size=8, 
             shape=21, fill="transparent") +
    geom_smooth(method='lm') +
    theme_ipsum()
#10. Plotting RBA Cash rate, yearly inflation, quarterly inflation
ggplot(Combi_df, aes(date)) + 
    geom_line(aes(y=djia, color = "djia")) +
    geom_line(aes(y=asx, color = "asx")) +
    xlab("Date") + ylab("ASX and DJIA Index's") + ggtitle("ASX 200 Vs DJIA") 
+ 
    annotate(geom="text", x=as.Date("2009-01-01"), y=7000, 
             label="Market Peak before 2008 GFC (Sep 2007)") +
    annotate(geom="text", x=as.Date("2010-01-01"), y=3200, 
             label="Market Trough (Jan 2009)") +
    annotate(geom="text", x=as.Date("2015-01-01"), y=10000, 
             label="Market drops due to Chinese Market fluctuations") +
    annotate(geom="text", x=as.Date("2019-01-01"), y=28000, 
             label="Tech Boom - FAANG") +
    coord_cartesian(clip = 'off') +
    annotate(geom="point", x=as.Date("2007-09-30"), y=6754, size=8, 
             shape=21, fill="transparent") +
    annotate(geom="point", x=as.Date("2009-01-31"), y=3400, size=8, 
             shape=21, fill="transparent") +
    annotate(geom="point", x=as.Date("2015-09-30"), y=16466, size=8, 
             shape=21, fill="transparent") +
    annotate(geom="point", x=as.Date("2015-09-30"), y=5200, size=8, 
             shape=21, fill="transparent") +
    annotate(geom="point", x=as.Date("2017-12-31"), y=26000, size=10, 
             shape=21, fill="transparent") +
    theme_ipsum()
#11. Plotting exchange rate
ggplot(Combi_df, aes(date, exchange_rate)) + geom_line() + 
    xlab("Date") + ylab("AUD/USD") + ggtitle("Australian Dollar/US Dollar 
Exchange Rate") +
    geom_smooth(method='lm') +
    theme_ipsum()
