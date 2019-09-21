## template for installing and loading multiple packages at once
for (package in c("tidyverse","here","skimr","janitor","magrittr","dplyr","reshape","moments","rsdmx","zoo","xts","Quandl","raustats","tidyquant","hydroTSM","openair","lubridate","matrixStats","psycho")) {
  if (!package %in% installed.packages()) {
    install.packages(package)
  }
  if (!package %in% .packages()) {
    library(package, character.only = TRUE)
  }
}

######## ANGUS's Code ########

# get some data ------
  
  (url <- "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/MEI_CLI/LOLITONO.AUS.M/all?startTime=2005-01&endTime=2019-07")
  
  dataset <- readSDMX(url)
  OECDLI <- as.data.frame(dataset)
  #Sort dates in xts
  date = seq(as.Date("2005-01-01"), by = "1 month", length.out = nrow(OECDLI))
  OECDLI <- xts(OECDLI[,-1], order.by = date, frequency = 1)
  #select data and label column
  OECDLI <-  setNames(OECDLI[,7], "oecd_li")
  
  
  (url <- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/MERCH_IMP/-.-1.-1.-.M/all?startTime=2005-01&endTime=2019-06")
  
  dataset <- readSDMX(url)
  AusImport <- as.data.frame(dataset)
  #Sort dates in xts
  date = seq(as.Date("2005-01-01"), by = "1 month", 
             length.out = nrow(AusImport))
  AusImport <- xts(AusImport[,-1], order.by = date, frequency = 1)
  #select data and label column
  AusImport <-  setNames(AusImport[,7], "abs_imports")
  
  (url <- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/MERCH_EXP/-.-1.-1.-.M/all?startTime=2005-01&endTime=2019-06")
  
  dataset <- readSDMX(url)
  AusExport <- as.data.frame(dataset)
  #Sort dates in xts
  date = seq(as.Date("2005-01-01"), by = "1 month", 
             length.out = nrow(AusExport))
  AusExport <- xts(AusExport[,-1], order.by = date, frequency = 1)
  #select data and label column
  AusExport <-  setNames(AusExport[,7], "abs_exports")
  
  # Merge Data ----
  
  Combi <- merge(OECDLI, AusImport, join="left")
  Combi <- merge(Combi, AusExport, join="left")
  CombiFrame <- as.data.frame(Combi)
  #CombiFrame <- mutate_all(CombiFrame, function(x) as.numeric(as.character(x)))





######## JOHN's Code ########
  
  #Gold Price: London Fixings, London Bullion Market Association (LBMA). Fixing levels are set per troy ounce. The London Gold Fixing Companies set the prices for gold that are globally considered as the international standard for pricing of gold. The Gold price in London is set twice a day by five LBMA Market Makers who comprise the London Gold Market Fixing Limited (LGMFL). The process starts with the announcement from the Chairman of the LGMFL to the other members of the LBMA Market Makers, then relayed to the dealing rooms where customers can express their interest as buyers or sellers and also the quantity they wish to trade. The gold fixing price is then set by collating bids and offers until the supply and demand are matched. At this point the price is announced as the 'Fixed' price for gold and all business is conducted on the basis of that price.
  gold_price_london_fixing <- Quandl("LBMA/GOLD", api_key="kf3rSrKM5xnKDzHNL74d")
  gold_price_london_fixing <- gold_price_london_fixing[order(as.Date(gold_price_london_fixing$Date, format="%Y/%m/%d")),]
  gold_price_london_fixing <- subset(gold_price_london_fixing, Date >= '2004-12-31') 
  gold_price_london_fixing <- subset(gold_price_london_fixing, Date <='2019-06-30')
  #Take the last date of each month
  gold_price_london_fixing <- gold_price_london_fixing %>%     
    mutate(gold_price = ymd(Date))%>%
    group_by(month = month(gold_price), year = year(gold_price)) %>%
    slice(which.max(day(gold_price))) %>%
    ungroup() %>%
    select(-month, -gold_price)
  #reorder sequentially by date
  gold_price_london_fixing <- gold_price_london_fixing[order(as.Date(gold_price_london_fixing$Date, format="%Y/%m/%d")),]
  #convert last day of the month to the first
  day(gold_price_london_fixing$Date) <- 1
  gold_price_london_fixing <- gold_price_london_fixing$`USD (AM)`
  Combi <- merge(Combi, gold_price_london_fixing, join="left")
  
  
  
  #UNEMPLOYMENT
  #Thousands of persons, ratios in percentage, and growth rates (all raw and seasonally adjusted). This new dataset builds on infra—annual labour market statistics currently published by the OECD. The new measures, with their relationships are 1. Working age population = Active population + Inactive population 2. Active population = Employed population + Unemployed population. The Short—Term Labour Market Statistics dataset contains predominantly quarterly labour statistics, and associated statistical methodological information, for the 34 OECD member countries and selected non—member economies. The Short—Term Labour Market Statistics dataset covers countries that compile labour statistics from sample household surveys on a monthly or quarterly basis. It is widely accepted that household surveys are the best source for labour market key statistics. In such surveys, information is collected from people living in households through a representative sample and the surveys are based on standard methodology and procedures used internationally. The subjects available cover: working age population by age; active and inactive labour force by age; employment by economic activity, by working time and by status; and, unemployment (including monthly harmonized unemployment) by age and by duration. Data is expressed in levels (thousands of persons) or rates (e.g. employment rate) where applicable. 
  #For more information see: http://stats.oecd.org/OECDStat_Metadata/ShowMetadata.ashx?Dataset=STLABOUR&Lang=en
  #https://www.quandl.com/data/OECD/STLABOUR_AUS_LRUN64TT_ST_M-Australia-Unemployment-Rate-Aged-15-64-All-Persons-Level-Rate-Or-Quantity-Series
  unemployment <- Quandl("OECD/STLABOUR_AUS_LRUN64TT_ST_M", api_key="kf3rSrKM5xnKDzHNL74d")
  unemployment <- unemployment[order(as.Date(unemployment$Date, format="%Y/%m/%d")),]
  unemployment <- subset(unemployment, Date >= '2004-12-31') 
  unemployment <- subset(unemployment, Date <='2019-06-30')
  unemployment <- unemployment %>%     
    mutate(unemployment = ymd(Date))%>%
    group_by(month = month(unemployment), year = year(unemployment)) %>%
    slice(which.max(day(unemployment))) %>%
    ungroup() %>%
    select(-month, -unemployment)
  unemployment <- unemployment[order(as.Date(unemployment$Date, format="%Y/%m/%d")),]
  day(unemployment$Date) <- 1
  unemployment <- unemployment$Value
  Combi <- merge(Combi, unemployment, join="left")
  


######## Charles' Code ########

  # list functions vailable from raustats package
  ls("package:raustats")
  
  # putting the cachelist to an array
  abslist <- abs_cat_cachelist
  
  # putting the cachelist to an array
  rbalist <- rba_cachelist
  
  ## Download datasets
  rba_mon <- rba_stats("A2")
  rba_infla <- rba_stats("G1")
  
  ### Data Munging ###
  {
    #### RBA Interest Rates datasets ####
    {
      colnames(rba_mon)
      unique(rba_mon$title)
      
      # Trim datasets
      col <- c('date','value','title')
      rba_mon <- rba_mon[,col]
      colnames(rba_mon)
      
      rba_mon <- subset(rba_mon, title == "New Cash Rate Target")
      col1 <- c('date','value')
      rba_mon <- rba_mon[,col1]
      
      # complete missing month
      rba_mon <- rba_mon %>% complete(date = seq.Date(min(date), max(date), by="month"))
      
      # take only data from 2005 onwards
      rba_mon <- subset(rba_mon, date >= '2005-01-01')
      
      # adding rate into first two months of 2005 becuase rate has not changed since dec 2003 which is 5.25
      rba_mon[1:2,2] = 5.25 
      
      # populate the rest of the NA
      rba_mon_fin <- rba_mon %>% fill('value')
      rba_mon_fin <- as.data.frame(rba_mon_fin)
      
    }
    
    #### RBA Year-end Inflation Datasets ####
    {
      colnames(rba_infla)
      unique(rba_infla$title)
      unique(rba_infla$frequency)
      rba_infla<- subset(rba_infla, title == "Year-ended inflation")
      unique(rba_infla$title)
      
      # Trim datasets
      col <- c('date','value','title')
      rba_infla <- rba_infla[,col]
      colnames(rba_infla)
      
      str(rba_infla)
      
      col1 <- c('date','value')
      rba_infla <- rba_infla[,col1]
      
      nrow(rba_infla)
      
      # complete missing month and put it on a new variable
      rba_infla_day <- rba_infla %>% complete(date = seq.Date(min(date), max(date), by="day"))
      
      # check to see confirm more rows created
      nrow(rba_infla_day)
      
      # populate the rest of the NA
      rba_infla_day <- rba_infla_day %>% fill('value')
      
      # check to confirm no na
      unique(is.na(rba_infla_day))
      
      # take only data from the last reading before 2005 onwards
      rba_infla_day <- subset(rba_infla_day, date >= '2005-01-01')
      
      # convert to monthly data
      rba_infla_day <- as.data.frame(rba_infla_day)
      
      rba_infla_day$date <- as.POSIXct.Date(rba_infla_day$date)
      rba_infla_day$date <- strptime(rba_infla_day$date,"%Y-%m-%d")
      rba_infla_day <- xts(rba_infla_day[,-1], order.by=rba_infla_day[,1])
      rba_infla_mon <- apply.monthly(rba_infla_day,mean)
      str(rba_infla_mon)
      
      rba_infla_mon<-as.data.frame(rba_infla_mon)
      
      nrow(rba_infla_mon)
      str(rba_infla_mon)
      rba_infla_mon$V1<- format(rba_infla_mon$V1, digits=1, nsmall=1)
      
      head(rba_infla_mon)
      tail(rba_infla_mon)
      nrow(rba_infla_mon)
      
      colnames(rba_infla_mon) <- c("Year-end Inflation")
      colnames(rba_infla_mon)
      
    }
    
    #### RBA Quarterly Inflation Datasets ####
    {
      # download datasets
      rba_infla_qrt <- rba_stats("G1")
      
      colnames(rba_infla_qrt)
      unique(rba_infla_qrt$title)
      unique(rba_infla_qrt$frequency)
      rba_infla_qrt<- subset(rba_infla_qrt, title == "Quarterly inflation")
      unique(rba_infla_qrt$title)
      
      # Trim datasets
      col <- c('date','value','title')
      rba_infla_qrt <- rba_infla_qrt[,col]
      colnames(rba_infla_qrt)
      
      str(rba_infla)
      
      col1 <- c('date','value')
      rba_infla_qrt <- rba_infla_qrt[,col1]
      
      nrow(rba_infla_qrt)
      
      # convert to daily readings
      rba_infla_qrt_day <- rba_infla_qrt %>% complete(date = seq.Date(min(date), max(date), by="day"))
      
      # check to see confirm more rows created
      nrow(rba_infla_qrt_day)
      
      # populate the rest of the NA on daily readings
      rba_infla_qrt_day <- rba_infla_qrt_day %>% fill('value')
      
      #confirm no NA
      unique(is.na(rba_infla_day))
      
      # take only data from the last reading before 2005 onwards
      rba_infla_qrt_day <- subset(rba_infla_qrt_day, date >= '2005-01-01')
      
      # convert to monthly data
      rba_infla_qrt_day <- as.data.frame(rba_infla_qrt_day)
      
      rba_infla_qrt_day$date <- as.POSIXct.Date(rba_infla_qrt_day$date)
      rba_infla_qrt_day$date <- strptime(rba_infla_qrt_day$date,"%Y-%m-%d")
      rba_infla_qrt_day <- xts(rba_infla_qrt_day[,-1], order.by=rba_infla_qrt_day[,1])
      rba_infla_qrt_mon <- apply.monthly(rba_infla_qrt_day,mean)
      str(rba_infla_qrt_mon)
      
      rba_infla_qrt_mon<-as.data.frame(rba_infla_qrt_mon)
      
      nrow(rba_infla_qrt_mon)
      str(rba_infla_qrt_mon)
      rba_infla_qrt_mon$V1 <- as.numeric(as.character(rba_infla_qrt_mon$V1))
      str(rba_infla_qrt_mon)
      summary(rba_infla_qrt_mon)
      rba_infla_qrt_mon$V1 <- round(rba_infla_qrt_mon$V1,1)
      
      
      head(rba_infla_qrt_mon)
      tail(rba_infla_qrt_mon)
      nrow(rba_infla_qrt_mon)
      
      colnames(rba_infla_qrt_mon) <- c("Quarterly Inflation")
      colnames(rba_infla_qrt_mon)
      
      
    }
  }
  
  ## Merge the three datasets
  {
    # list all the datasets
    head(rba_mon_fin)
    tail(rba_mon_fin)
    head(rba_infla_mon)
    head(rba_infla_qrt_mon)
    
    # check row numbers for all the datasets
    nrow(rba_mon_fin)
    nrow(rba_infla_mon)
    nrow(rba_infla_qrt_mon)
    
    # summary & str
    str(rba_infla_mon)
    
    # sort date in xts for rba_mon_fin
    date <- seq(as.Date("2005-01-01/2019-06-01"), by = "1 month", 
                length.out = nrow(rba_mon_fin))
    rba_mon_fin <- xts(rba_mon_fin[,-1], order.by = date, frequency = 1) 
    
    # cut off excesses date range and put in the correct date range
    rba_mon_fin <- rba_mon_fin["2005-01-01/2019-06-01"]
    
    # sort date in xts for rba_infla_mon
    date <- seq(as.Date("2005-01-01"), by = "1 month",length.out = nrow(rba_mon_fin))
    rba_infla_mon <- xts( x = rba_infla_mon, order.by = date)
    rba_infla_mon <- as.xts(rba_infla_mon)
    # rba_infla_mon <- xts(rba_infla_mon[,-1], order.by = date, frequency = 1) 
    
    # sort date in xts for rba_infla_qrt_mon
    date <- seq(as.Date("2005-01-01/2019-06-01"), by = "1 month", 
                length.out = nrow(rba_infla_qrt_mon))
    rba_infla_qrt_mon <- xts(rba_infla_qrt_mon, order.by = date, frequency = 1) 
    
    # merge with the consolidated datasets
    Combi <- merge(Combi, rba_mon_fin, join="left")
    Combi <- merge(Combi, rba_infla_mon, join="left")
    Combi <- merge(Combi, rba_infla_qrt_mon, join="left")
    
    colnames(Combi)
    
    ## correcting colnames
    # rename(Combi$rba_mon_fin, "RBA")
    # dimnames(Combi$rba_mon_fin) <- "RBA interest rates"
    # colnames(Combi[,8]) <- c("RBA interest rates")
    # colnames(Combi$rba_mon_fin) <- "RBA interest rates" 
    colnames(Combi)
    
    # Changing colname one by one
    # names(Combi)[8]<- "RBA Cash Rate"
    
  }

  ## Vincent's code

# Exchange rate monthly
{
  
  # Get dataframe combine
  df_combi = as.data.frame(Combi)
  df_combi['Date'] <- as.Date(rownames(df_combi), "%Y-%m-%d")
  
  # Source: https://www.rba.gov.au/statistics/historical-data.html
  read_exchange_rate <- function(file, exchange_rate_all) {
    exchange_rate <- read_xls(file, skip = 10)
    names(exchange_rate)
    colnames(exchange_rate)[colnames(exchange_rate)=="Series ID"] <- "Date"
    colnames(exchange_rate)[colnames(exchange_rate)=="FXRUSD"] <- "Aud_usd"
    names(exchange_rate)
    
    exchange_rate <- exchange_rate %>% select (c(Date, Aud_usd))
    
    exchange_rate$Date <- as.Date(exchange_rate$Date, "%Y-%m-%d", tz = "Australia/Sydney")
    exchange_rate$Aud_usd = as.numeric(exchange_rate$Aud_usd)
    
    exchange_rate_all <- rbind(exchange_rate_all, exchange_rate)
    return(exchange_rate_all)
  }
  
  # read all files
  exchange_rate_all <- NULL
  exchange_rate_all <- read_exchange_rate("data/f11hist-1969-2009.xls", exchange_rate_all)
  exchange_rate_all <- read_exchange_rate("data/f11hist.xls", exchange_rate_all)

  
  # Extract month year of oil other and data combine to make it a key to join
  exchange_rate_all$Month_Year = format(exchange_rate_all$Date, "%m-%Y")
  exchange_rate_all <- exchange_rate_all %>% select(-matches("Date"))
  df_combi$Month_Year = format(df_combi$Date, "%m-%Y")
  
  # Merge by month and year
  df_combi <- df_combi %>% 
    merge(exchange_rate_all, by = 'Month_Year', all.x = TRUE)
  df_combi <- df_combi[order(df_combi$Date),]
  
  # convert it back to Combi
  rownames(df_combi) <- df_combi$Date
  df_combi <- df_combi %>% select(-matches("Date"))
  Combi <- as.xts(df_combi)

}
  
# Oil data
{
  
  # # Get dataframe combine
  # df_combi = as.data.frame(Combi)
  # df_combi['Date'] <- as.Date(rownames(df_combi), "%Y-%m-%d")
  # 
  # # Read csv oil data
  # oil_other <- read.csv("data/2005_2019_asx_DJIA_PE_Yield_Iron_Oil.csv")
  # 
  # # Extract month year of oil other and data combine to make it a key to join
  # oil_other <- oil_other %>% 
  #                 separate_(col = "date", into = c("Day", "Month", "Year"), sep = "/") %>%
  #                 unite(Month_Year, Month, Year, sep = "-") %>%
  #                 select(-matches("Day"))
  # 
  # df_combi <- df_combi %>% 
  #   separate_(col = "Date", into = c("Year", "Month", "Day"), sep = "-", remove = FALSE) %>%
  #   unite(Month_Year, Month, Year, sep = "-") %>%
  #   select(-matches("Day"))
  # 
  # # Merge by month and year
  # df_combi <- df_combi %>% 
  #             merge(oil_other, by = 'Month_Year', all.x = TRUE)
  # df_combi <- df_combi[order(df_combi$Date),]
  # 
  # # convert it back to Combi
  # rownames(df_combi) <- df_combi$Date
  # df_combi_t <- df_combi %>% select(-matches("Date"))
  # Combi <- as.xts(df_combi_t)
  
  
  ##### Lawrence's code ##### --------
  # Oil data
  # Read csv oil data
  oil_other <- read.csv("data/2005_2019_asx_DJIA_PE_Yield_Iron_Oil.csv")
  #Sort dates in xts
  date = seq(as.Date("2005-01-01"), by = "1 month", length.out = nrow(oil_other))
  oil_other <- xts(oil_other[,-1], order.by = date, frequency = 1)
  as.numeric(oil_other)
  oil_other <- as.xts(oil_other)
  # Merge into combi
  Combi <- merge(Combi, oil_other, join="left")
  Combi[,11] <- oil_other[,1]
  Combi[,12] <- oil_other[,2]
  Combi[,13] <- oil_other[,3]
  Combi[,14] <- oil_other[,4]
  Combi[,15] <- oil_other[,5]
  Combi[,16] <- oil_other[,6]
  
  df_combi = as.data.frame(Combi)
  df_combi['Date'] <- as.Date(rownames(df_combi), "%Y-%m-%d")
}

head(Combi)
head(df_combi)

df_combi %>% count()

ggplot(data = df_combi) +
  geom_histogram(mapping = aes(x = Date), binwidth = 5)

write.csv(df_combi,'./data-clean/final_file.csv', row.names = FALSE)
colnames(Combi)

##### Data Cleaning ####
# Changing colname alltogether

names(Combi) <- c("Month_Year","oecd_li","abs_imports",
                  "abs_exports","gold_price_london_fixing",
                  "unemployment","rba_cash_rate",
                  "yearly_inflation","quarterly_inflation",
                  "exchange_rate","asx","djia","pe_ratio",
                  "dividend","iron","oil")
colnames(Combi)

# reorder column, putting asx in the front and removing "Month_Year"
Combi <- Combi[,c(11,2,3,4,5,6,7,8,9,10,12,13,14,15,16)]

colnames(Combi)
nrow(Combi)


##### Feature Engineering #####

# temporary - remove exchange rate NA
colnames(Combi)
#Combi <- Combi[,c(1,2,3,4,5,6,7,8,9,11,12,13,14,15)]
# drop July
Combi <- Combi[-nrow(Combi),] 

# Create MOM% Changes --------
x <- as.xts(Combi)
na.locf(x, fromLast = TRUE) 
p <- matrix(0, nrow(x), ncol(x))
#Create a loop for row and columns
for (j in 1:ncol(x)) {
  MOMtemp <- matrix(periodReturn(x[,j],period='monthly',subset='2004::'))
  p[,j] <- MOMtemp
}
#add back date index in xts
date = seq(as.Date("2005-01-01"), by = "1 month", length.out = nrow(p))
p_xts <- xts(p, order.by = date, frequency = 1)

# Re-add columns that dont need MOM% ie already detrended
p_xts[,2] <- x[,2]
p_xts[,6] <- x[,6]
p_xts[,7] <- x[,7]
p_xts[,8] <- x[,8]
p_xts[,9] <- x[,9]
p_xts[,12] <- x[,12]
p_xts[,13] <- x[,13]

# Add binary 0 and 1 for ASX on prior month
p_xts_df <- as.data.frame(p_xts)
p_xts_df$up_down <- replace(p_xts_df$V1, which(p_xts_df$V1 <= 0), 0)
p_xts_df$up_down <- replace(p_xts_df$up_down, which(p_xts_df$up_down > 0), 1)


# Z-score dataframe --------
Combi_zs <- as.data.frame(p_xts_df)
Combi_zs <-  Combi_zs %>% 
  psycho::standardize() 


names(Combi_zs) <- c("asx","oecd_li","abs_imports",
                  "abs_exports","gold_price_london_fixing",
                  "unemployment","rba_cash_rate",
                  "yearly_inflation","quarterly_inflation",
                  "exchange_rate","djia","pe_ratio",
                  "dividend","iron","oil","binary_asx")
colnames(Combi_zs)

# reorder column, putting asx in the front and removing "Month_Year"
Combi_eng <- Combi_zs[,c(16,2,3,4,5,6,7,8,9,10,11,12,13,14,15)]

install.packages("hrbrthemes")
library(hrbrthemes)

#John EDA
Combi_df <- data.frame(date=index(Combi), coredata(Combi))
View(Combi_df)
Combi_df$date <- as.Date(Combi_df$date)
str(Combi_df)

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
  coord_cartesian(clip = 'off') +
  annotate(geom="point", x=as.Date("2009-01-31"), y=40, size=8, shape=21, fill="transparent") +
  annotate(geom="point", x=as.Date("2015-01-31"), y=50, size=8, shape=21, fill="transparent") +
  geom_smooth(method='lm') +
  theme_ipsum()

#Gold price
ggplot(Combi_df, aes(date, gold_price_london_fixing)) + geom_line() + 
  xlab("Date") + ylab("ASX Index") + ggtitle("Price of Gold Over time") + 
  annotate(geom="text", x=as.Date("2008-01-01"), y=1000, 
           label="ASX 200 Trough GFC") +
  annotate(geom="text", x=as.Date("2007-01-01"), y=550, 
           label="Peak of Market prior to GFC") +
  coord_cartesian(clip = 'off') +
  annotate(geom="point", x=as.Date("2009-01-31"), y=930, size=8, shape=21, color ="orange", fill="transparent") +
  annotate(geom="point", x=as.Date("2007-07-31"), y=650, size=8, shape=21, color ="orange", fill="transparent") +
  geom_smooth(method='lm') +
  theme_ipsum()

#ASX/DJIA
ggplot(Combi_df, aes(date, djia)) + geom_line() + 
  xlab("Date") + ylab("ASX Index") + ggtitle("Price of Gold Over time") + 
  annotate(geom="text", x=as.Date("2008-01-01"), y=1000, 
           label="ASX 200 Trough GFC") +
  annotate(geom="text", x=as.Date("2007-01-01"), y=550, 
           label="Peak of Market prior to GFC") +
  coord_cartesian(clip = 'off') +
  annotate(geom="point", x=as.Date("2009-01-31"), y=930, size=8, shape=21, color ="orange", fill="transparent") +
  annotate(geom="point", x=as.Date("2007-07-31"), y=650, size=8, shape=21, color ="orange", fill="transparent") +
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
  coord_cartesian(clip = 'off') +
  annotate(geom="point", x=as.Date("2007-09-30"), y=6754, size=8, shape=21, fill="transparent") +
  annotate(geom="point", x=as.Date("2009-01-31"), y=3400, size=8, shape=21, fill="transparent") +
  annotate(geom="point", x=as.Date("2015-09-30"), y=16466, size=8, shape=21, fill="transparent") +
  annotate(geom="point", x=as.Date("2015-09-30"), y=5200, size=8, shape=21, fill="transparent") +
  theme_ipsum()


## Code by Lawrence Lam for AT 2B (i) - Class Presentation - EDA
## ----------------------------------------------------------------

library(tidyverse)

LL1 <- Combi_zs


# Have a feel of the imported tibble

glimpse(LL1)
names(LL1)
summary(LL1)


## Check variations within each variable - get to know the data
## ------------------------------------------------------------

# Check NA for all 16 variables

is.na(LL1)

LL1%>% 
  summarise(sum(is.na(LL1)))


# Check summary statistics and distributions (with 0.1 binwidth) of each variable

LL1%>%
  summarise(mean=mean(asx),sd=sd(asx),median=median(asx),min=min(asx),max=max(asx))

ggplot(LL1)+
  geom_histogram(aes(asx),binwidth=0.1,colour="gold",fill="dark green")



LL1%>%
  summarise(mean=mean(oecd_li),sd=sd(oecd_li),median=median(oecd_li),
            min=min(oecd_li),max=max(oecd_li))

ggplot(LL1)+
  geom_histogram(aes(oecd_li),binwidth=0.1,colour="gold",fill="dark green")



LL1%>%
  summarise(mean=mean(abs_imports),sd=sd(abs_imports),median=median(abs_imports),
            min=min(abs_imports),max=max(abs_imports))

ggplot(LL1)+
  geom_histogram(aes(abs_imports),binwidth=0.1,colour="gold",fill="dark green")


LL1%>%
  summarise(mean=mean(abs_exports),sd=sd(abs_exports),median=median(abs_exports),
            min=min(abs_exports),max=max(abs_exports))
ggplot(LL1)+
  geom_histogram(aes(abs_exports),binwidth=0.1,colour="gold",fill="dark green")



LL1%>%
  summarise(mean=mean(gold_price_london_fixing),sd=sd(gold_price_london_fixing),
            median=median(gold_price_london_fixing),
            min=min(gold_price_london_fixing),max=max(gold_price_london_fixing))

ggplot(LL1)+
  geom_histogram(aes(gold_price_london_fixing),binwidth=0.1,colour="gold",fill="dark green")


LL1%>%
  summarise(mean=mean(unemployment),sd=sd(unemployment),
            median=median(unemployment),
            min=min(unemployment),max=max(unemployment))

ggplot(LL1)+
  geom_histogram(aes(unemployment),binwidth=0.1,colour="gold",fill="dark green")



LL1%>%
  summarise(mean=mean(rba_cash_rate),sd=sd(rba_cash_rate),
            median=median(rba_cash_rate),
            min=min(rba_cash_rate),max=max(rba_cash_rate))

ggplot(LL1)+
  geom_histogram(aes(rba_cash_rate),binwidth=0.1,colour="gold",fill="dark green")



LL1%>%
  summarise(mean=mean(yearly_inflation),sd=sd(yearly_inflation),
            median=median(yearly_inflation),
            min=min(yearly_inflation),max=max(yearly_inflation))

ggplot(LL1)+
  geom_histogram(aes(yearly_inflation),binwidth=0.1,colour="gold",fill="dark green")



LL1%>%
  summarise(mean=mean(quarterly_inflation),sd=sd(quarterly_inflation),
            median=median(quarterly_inflation),
            min=min(quarterly_inflation),max=max(quarterly_inflation))

ggplot(LL1)+
  geom_histogram(aes(quarterly_inflation),binwidth=0.1,colour="gold",fill="dark green")



LL1%>%
  summarise(mean=mean(exchange_rate),sd=sd(exchange_rate),
            median=median(exchange_rate),
            min=min(exchange_rate),max=max(exchange_rate))

ggplot(LL1)+
  geom_histogram(aes(exchange_rate),binwidth=0.1,colour="gold",
                 fill="dark green") 


LL1%>%
  summarise(mean=mean(djia),sd=sd(djia),median=median(djia),
            min=min(djia),max=max(djia))

ggplot(LL1)+
  geom_histogram(aes(djia),binwidth=0.1,colour="gold",fill="dark green")


LL1%>%
  summarise(mean=mean(pe_ratio),sd=sd(pe_ratio),median=median(pe_ratio),
            min=min(pe_ratio),max=max(pe_ratio))

ggplot(LL1)+
  geom_histogram(aes(pe_ratio),binwidth=0.1,colour="gold",fill="dark green")



LL1%>%
  summarise(mean=mean(dividend),sd=sd(dividend),median=median(dividend),
            min=min(dividend),max=max(dividend))

ggplot(LL1)+
  geom_histogram(aes(dividend),binwidth=0.1,colour="gold",fill="dark green")



LL1%>%
  summarise(mean=mean(iron),sd=sd(iron),median=median(iron),
            min=min(iron),max=max(iron))

ggplot(LL1)+
  geom_histogram(aes(iron),binwidth=0.1,colour="gold",fill="dark green")



LL1%>%
  summarise(mean=mean(oil),sd=sd(oil),median=median(oil),
            min=min(oil),max=max(oil))

ggplot(LL1)+
  geom_histogram(aes(oil),binwidth=0.1,colour="gold",fill="dark green")



LL1%>%
  summarise(mean=mean(binary_asx),sd=sd(binary_asx),
            median=median(binary_asx),min=min(binary_asx),max=max(binary_asx))

ggplot(LL1)+
  geom_histogram(aes(binary_asx),binwidth=0.1,colour="gold",fill="dark green")



## Check covariations between "ASX" and the other 14 predictors
## ---------------------------------------------------------

# Set up a function to calculate r coefficient between the predictor and ASX

r<- function(r){cor(r,LL1$asx)}

# ------------------------------------
ggplot(LL1,aes(oecd_li,asx))+
  geom_point(colour="blue")+
  geom_smooth(method="lm")

r(LL1$oecd_li)


# My poor function could not xlab the predictor along the x axis
# scatter <- function(p){ggplot(LL1,aes(p,asx))+
#   geom_point(colour="blue")+
#   geom_smooth(method="lm")+
#   xlab("p")}

# ------------------------------------------------
ggplot(LL1,aes(abs_imports,asx))+
  geom_point(colour="blue")+
  geom_smooth(method="lm")

r(LL1$abs_imports)

# ------------------------------------------------
ggplot(LL1,aes(abs_exports,asx))+
  geom_point(colour="blue")+
  geom_smooth(method="lm")

r(LL1$abs_exports)

# ------------------------------------------------
ggplot(LL1,aes(gold_price_london_fixing,asx))+
  geom_point(colour="blue")+
  geom_smooth(method="lm")

r(LL1$gold_price_london_fixing)

# ------------------------------------------------
ggplot(LL1,aes(unemployment,asx))+
  geom_point(colour="blue")+
  geom_smooth(method="lm")

r(LL1$unemployment)

# ------------------------------------------------
ggplot(LL1,aes(rba_cash_rate,asx))+
  geom_point(colour="blue")+
  geom_smooth(method="lm")

r(LL1$rba_cash_rate)

# ------------------------------------------------
ggplot(LL1,aes(yearly_inflation,asx))+
  geom_point(colour="blue")+
  geom_smooth(method="lm")

r(LL1$yearly_inflation)

# -------------------------------------------------
ggplot(LL1,aes(quarterly_inflation,asx))+
  geom_point(colour="blue")+
  geom_smooth(method="lm")

r(LL1$quarterly_inflation)

# -------------------------------------------------
ggplot(LL1,aes(exchange_rate,asx))+
  geom_point(colour="blue")+
  geom_smooth(method="lm")

r(LL1$exchange_rate)

# -------------------------------------------------
ggplot(LL1,aes(djia,asx))+
  geom_point(colour="blue")+
  geom_smooth(method="lm")

r(LL1$djia)

# -------------------------------------------------
ggplot(LL1,aes(pe_ratio,asx))+
  geom_point(colour="blue")+
  geom_smooth(method="lm")

r(LL1$pe_ratio)

# -------------------------------------------------
ggplot(LL1,aes(dividend,asx))+
  geom_point(colour="blue")+
  geom_smooth(method="lm")

r(LL1$dividend)

# -------------------------------------------------
ggplot(LL1,aes(iron,asx))+
  geom_point(colour="blue")+
  geom_smooth(method="lm")

r(LL1$iron)

# -------------------------------------------------
ggplot(LL1,aes(oil,asx))+
  geom_point(colour="blue")+
  geom_smooth(method="lm")

r(LL1$oil)

## Further analysis of dataset - pairs, corrplot, factor analysis
## ---------------------------------------------------------------


leading <- pairs(~asx+oecd_li+abs_imports+abs_exports+rba_cash_rate,
                 data=LL1,main="Leading Indicators")

concurrent <- pairs(~asx+gold_price_london_fixing+exchange_rate+djia+iron+oil,
                    data=LL1, main="Concurrent Indicators")

lagging <- pairs(~asx+unemployment+yearly_inflation+quarterly_inflation+
                   pe_ratio+dividend,data=LL1,main="Lagging Indicators")

# -----------------------------------------------------------------

library(corrplot)

corrplot(cor(LL1),method="number")

corrplot(cor(LL1),method="circle")

# -----------------------------------------------------------------


library(psych)
library(GPArotation)

sixfactor <- fa(LL1,nfactors=6,rotate="oblimin",fm="minres")
print(sixfactor)


## Initial 2 multiple linear regressions based on corrplot correlation 
## --------------------------------------------------------------------


library(rsdmx)
ml1 <- lm(asx ~ yearly_inflation+exchange_rate+djia+pe_ratio+dividend+oil,
          data=LL1,na.action=na.omit)

summary(ml1)

plot(ml1)



ml2 <- lm(asx ~ yearly_inflation*exchange_rate*djia*pe_ratio*dividend*oil,
          data=LL1, na.action=na.omit)

summary(ml2)

plot(ml2)

## ---------------------END-------------------------------------------------
