## template for installing and loading multiple packages at once
for (package in c("tidyverse","here","skimr","janitor","magrittr","dplyr","reshape","moments","rsdmx","zoo","xts","Quandl","raustats","tidyquant","hydroTSM","openair","lubridate","matrixStats","psycho","Amelia","corrplot","GPArotation","aTSA")) {
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

#### logistic regression -------

# shift dataframe function
shift <- function(x, n){
  c(x[-(seq(n))], rep(NA, n))
}

# Move all variables back one column
Shift_Combi <- Combi_eng
for (j in 1:ncol(Shift_Combi)) {
  Shift_Combi[,j] <- shift(Shift_Combi[,j],1)
  
}

Shift_Combi[,1] <- Combi_eng[,1]
# Take off last column
Shift_Combi <- Shift_Combi[-nrow(Shift_Combi),]

#take a copy for 4 clust series
Shift_Combi_4clust <- Shift_Combi

# Separate into train and test sets
Shift_Combi$id <- 1:nrow(Shift_Combi) 
Shift_Combi.train <- Shift_Combi %>% dplyr::sample_frac(.75)
Shift_Combi.test  <- dplyr::anti_join(Shift_Combi, Shift_Combi.train, by = 'id')

# Take off id columns
Shift_Combi.train <- Shift_Combi.train[,-ncol(Shift_Combi.train)] 
Shift_Combi.test <- Shift_Combi.test[,-ncol(Shift_Combi.test)] 

# Logistic regression with train set
glm1 = glm(binary_asx ~ ., family=binomial(logit), data = Shift_Combi.train)
summary(glm1)
plot(glm1)

# predict with train set
probability<-predict(glm1,newdata = Shift_Combi.test,type="response")
head(probability)

#setting a threshold value of 0.5 for positive...
#you may want to see if there are better settings that you 
#could use here (Hint: do a search for "ROC curve")
prediction <- ifelse(probability > 0.5, 1, 0) 
# building a contingency table of the counts at each combination of factor levels
confusion  <- table(Shift_Combi.test$asx_binary, prediction) 
confusion 

# Compute RoC curve and AUC for Shift_Combi.test data

pred <- predict(glm1, newdata=Shift_Combi.test)
pred_y <- as.numeric(pred > 0)
true_y <- as.numeric(Shift_Combi.test$binary_asx==1)
true_pos <- (true_y==1) & (pred_y==1)
true_neg <- (true_y==0) & (pred_y==0)
false_pos <- (true_y==0) & (pred_y==1)
false_neg <- (true_y==1) & (pred_y==0)
conf_mat <- matrix(c(sum(true_pos), sum(false_pos),
                     sum(false_neg), sum(true_neg)), 2, 2)

idx <- order(-pred)
recall <- cumsum(true_y[idx]==1)/sum(true_y==1)
recall
specificity <- (sum(true_y==0) - cumsum(true_y[idx]==0)
)/sum(true_y==0)
specificity
roc_df <- data.frame(recall = recall, specificity = specificity)
roc_df
ggplot(roc_df, aes(x=specificity, y=recall)) +
  geom_line(colour="blue") +
  scale_x_reverse(expand=c(0, 0)) +
  scale_y_continuous(expand=c(0, 0)) +
  geom_line(data=data.frame(x=(0:100)/100), aes(x=x, y=1-x),
            linetype='dotted', color='red')

sum(roc_df$recall[-1] * diff(1-roc_df$specificity))

#### linear model
glm1 <- glm(binary_asx ~ ., data = Shift_Combi.train, family=gaussian(link = "identity"))
summary(glm1)

# Rerun with cluster data ----------

#take asx_binary, oecd_li, quarterly_inflation, dividend yield
Shift_Combi_4clust <- Shift_Combi_4clust[,c(1,2,9,11,13)] 

# Separate into train and test sets
Shift_Combi_4clust$id <- 1:nrow(Shift_Combi_4clust) 
Shift_Combi_4clust.train <- Shift_Combi_4clust %>% dplyr::sample_frac(.75)
Shift_Combi_4clust.test  <- dplyr::anti_join(Shift_Combi_4clust, Shift_Combi_4clust.train, by = 'id')

# Take off id columns
Shift_Combi_4clust.train <- Shift_Combi_4clust.train[,-ncol(Shift_Combi_4clust.train)] 
Shift_Combi_4clust.test <- Shift_Combi_4clust.test[,-ncol(Shift_Combi_4clust.test)] 

# Logistic regression 4clust with train set
glm1 = glm(binary_asx ~ ., family=binomial(logit), data = Shift_Combi_4clust.train)
summary(glm1)
plot(glm1)

# predict with train set
probability<-predict(glm1,newdata = Shift_Combi_4clust.test,type="response")
head(probability)

#setting a threshold value of 0.5 for positive...
#you may want to see if there are better settings that you 
#could use here (Hint: do a search for "ROC curve")
prediction <- ifelse(probability > 0.5, 1, 0) 
# building a contingency table of the counts at each combination of factor levels
confusion  <- table(Shift_Combi_4clust.test$asx_binary, prediction) 
confusion 

# Compute RoC curve and AUC for Shift_Combi.test data

pred <- predict(glm1, newdata=Shift_Combi_4clust.test)
pred_y <- as.numeric(pred > 0)
true_y <- as.numeric(Shift_Combi_4clust.test$binary_asx==1)
true_pos <- (true_y==1) & (pred_y==1)
true_neg <- (true_y==0) & (pred_y==0)
false_pos <- (true_y==0) & (pred_y==1)
false_neg <- (true_y==1) & (pred_y==0)
conf_mat <- matrix(c(sum(true_pos), sum(false_pos),
                     sum(false_neg), sum(true_neg)), 2, 2)

idx <- order(-pred)
recall <- cumsum(true_y[idx]==1)/sum(true_y==1)
recall
specificity <- (sum(true_y==0) - cumsum(true_y[idx]==0)
)/sum(true_y==0)
specificity
roc_df <- data.frame(recall = recall, specificity = specificity)
roc_df
ggplot(roc_df, aes(x=specificity, y=recall)) +
  geom_line(colour="blue") +
  scale_x_reverse(expand=c(0, 0)) +
  scale_y_continuous(expand=c(0, 0)) +
  geom_line(data=data.frame(x=(0:100)/100), aes(x=x, y=1-x),
            linetype='dotted', color='red')

sum(roc_df$recall[-1] * diff(1-roc_df$specificity))


##--------- EDA ------------ #

create_report(Combi)

install.packages("dataMeta")
library(dataMeta)

##### Building up data dictionary ####
{
  # list functions vailable from a package
  ls("package:dataMeta")
  
  ?build_dict
  ?build_linker
  
  colnames(Combi)
  nrow(Combi)
  Combi_df <- as.data.frame(Combi)
  colnames(Combi_df)
  
  variable_desc <- c("ASX 200 Index","OECD Leading Indicators","Import Values from ABS (AUD)","Export Values from ABS (AUD)","LBMA Gold Price (USD)","Unemployment rate (%)","RBA Cash Rate target as announced (%)","Inflation rate calculated over the last 12 months (%)","Inflation rate calculated over the last 3 months (%)","USD vs AUD","Dow Jones Industrial Index","Price to Earning Ratio","Dividend Yield as percentage (%)","Iron Price (AUD) ","Oil Price (AUD)")
  
  var_type <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
  
  my.data <- Combi_df
  
  link <- build_linker(my.data = my.data, variable_description = variable_desc, variable_type = var_type)
  
  link
  
  dictionary <- build_dict(my.data = my.data, linker = link, option_description = NULL, prompt_varopts = FALSE)
  dictionary
  
  # change column name
  colnames(dictionary) <- c("Variable Name","Variable Description", "Range of values")
  
  dictionary
  
  
  install.packages("formattable")
  library(formattable)
  
  formattable(dictionary)
  
  ls("package:formattable")
  
  customGreen = "#71CA97"
  customGreen0 = "#a3f6a4"
  customGreen1 = "#16d21a"
  customGreen2 = "#074608"
  customRed = "#ff7f7f"
  customBlue = "#111ba3"
  
  improvement_formatter <- 
    formatter("span", 
              style = x ~ style(
                font.weight = "bold", 
                color = ifelse(x > 0, customGreen1, ifelse(x < 0, customRed, "black"))))
  
  formattable(dictionary, align = c("l","l","r"), list(
    `Variable Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
    `Range of values` = improvement_formatter))
  
  ?color_tile
  
  ##### POST FEATURE ENGINEERING #####
  
  Combi_eng_dict <- Combi_eng[,c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)]
  
  colnames(Combi_eng)
  colnames(Combi_eng_dict)
  
  
  variable_desc1 <- c("Changes in ASX index 1 for up and 0 for down"," Z - Score in OECD Leading Indicators","Z - Score in Import Values from ABS"," Z - Score in Export Values from ABS","Z - Score in LBMA Gold Price"," Z - Score in Unemployment rate","Z - Score in RBA Cash Rate target as announced (%)","Z - Score in Inflation rate calculated over the last 12 months","Z - Score in Inflation rate calculated over the last 3 months","Z - Score in USD vs AUD","Z - Score in Dow Jones Industrial Index","Z - Score in Price to Earning Ratio","Z - Score in Dividend Yield as percentage","Z - Score in Iron Price","Z - Score in Oil Price")
  
  
  my.data1 <- Combi_eng_dict
  
  link1 <- build_linker(my.data = my.data1, variable_description = variable_desc1, variable_type = var_type)
  
  link1
  
  dictionary1 <- build_dict(my.data = my.data1, linker = link1, option_description = NULL, prompt_varopts = FALSE)
  dictionary1
  
  colnames(dictionary1) <- c("Variable Name","Variable Description", "Range of values")
  
  formattable(dictionary1, align = c("l","l","r"), list(
    `Variable Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
    `Range of values` = improvement_formatter))
  
  
  ##### source #####
  # https://www.littlemissdata.com/blog/prettytables
  
}

##### EDA pre Featre Engineering #####
{
  Combi_df <- data.frame(date=index(Combi), coredata(Combi))
  Combi_df$date <- as.Date(Combi_df$date)
  colnames(Combi_df)
  #Plotting rba_cash_rate
  ggplot(Combi_df, aes(date, rba_cash_rate)) + geom_line() +
    xlab("Date") + ylab("RBA Cash Rate") + ggtitle("Cash Rate Target as announced by the RBA") +
    annotate(geom="text", x=as.Date("2012-01-01"), y=6,
             label="Lehman Brothers filed for bankruptcy") +
    annotate(geom="text", x=as.Date("2011-06-01"), y=7.25,
             label="Peaked Cash Rate before GFC") +
    annotate(geom="text", x=as.Date("2006-10-01"), y=3.25,
             label="Fifth consecutive rate cut") +
    annotate(geom="text", x=as.Date("2014-12-15"), y=4.75,
             label="China's growth started to slow down") +
    annotate(geom="text", x=as.Date("2010-05-07"), y=2.5,
             label="Mining investment has slowed down") +
    annotate(geom="text", x=as.Date("2014-08-07"), y=1.5,
             label="Start of a stable rate") +
    coord_cartesian(clip = 'off') +
    annotate(geom="point", x=as.Date("2008-09-30"), y=6, size=8, shape=21, fill="transparent") +
    annotate(geom="point", x=as.Date("2008-08-01"), y=7.25, size=8, shape=21, fill="transparent") +
    annotate(geom="point", x=as.Date("2009-02-01"), y=3.25, size=8, shape=21, fill="transparent") +
    annotate(geom="point", x=as.Date("2011-10-01"), y=4.75, size=8, shape=21, fill="transparent") +
    annotate(geom="point", x=as.Date("2013-08-07"), y=2.5, size=8, shape=21, fill="transparent") +
    annotate(geom="point", x=as.Date("2016-08-07"), y=1.5, size=8, shape=21, fill="transparent") +
    geom_smooth(method='lm') +
    theme_ipsum()
  
  #Plotting yearly inflation rate
  ggplot(Combi_df, aes(date, yearly_inflation)) + geom_line() +
    xlab("Date") + ylab("Yearly Inflation") + ggtitle("Year-ended Consumer Price Inflation") +
    annotate(geom="text", x=as.Date("2011-08-30"), y=5,
             label="GFC started affecting inflation") +
    annotate(geom="text", x=as.Date("2012-02-28"), y=3.7,
             label="Low confidence affected by GFC") +
    annotate(geom="text", x=as.Date("2011-04-30"), y=1.2,
             label="Credit growth") +
    annotate(geom="text", x=as.Date("2014-08-30"), y=1,
             label="Lowest inflation rate") +
    coord_cartesian(clip = 'off') +
    annotate(geom="point", x=as.Date("2008-11-30"), y=5, size=8, shape=21, fill="transparent") +
    annotate(geom="point", x=as.Date("2009-02-28"), y=3.7, size=8, shape=21, fill="transparent") +
    annotate(geom="point", x=as.Date("2009-11-30"), y=1.2, size=8, shape=21, fill="transparent") +
    annotate(geom="point", x=as.Date("2016-08-30"), y=1, size=8, shape=21, fill="transparent") +
    geom_smooth(method='lm') +
    theme_ipsum()
  
  Combi_df[,c(1,9,10)]
  
  #Plotting quarterly and yearly inflation rate
  colnames(Combi_df)
  Infla <- Combi_df[,c(1,9,10)]
  Infla_long <- melt(Infla, id="date")  # convert to long format
  ggplot(data=Infla_long,
         aes(x=date, y=value, colour=variable)) +
    geom_line()
  
  # source https://stackoverflow.com/questions/3777174/plotting-two-variables-as-lines-using-ggplot2-on-the-same-graph
  
  
}

##### EDA post Feature Engineering ####

Combi_eng$date <- seq(as.Date("2005-01-01"), by = "month", length.out = 174)

ncol(Combi_eng)
colnames(Combi_eng)

Combi_eng <- Combi_eng[,c(16,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)]

Combi_eng_df <- Combi_eng

Combi_eng_df <- data.frame(date=index(Combi_eng), coredata(Combi_eng))
Combi_eng_df$date <- as.Date(Combi_eng_df$date)
colnames(Combi_eng_df)

install.packages("reshape2")
library(reshape2)

colnames(Combi_eng_df)

cluster1 <- Combi_eng_df[,c(1,12,11,16)]

cluster1_long <- melt(cluster1, id="date")  # convert to long format

colnames(cluster1_long)

ggplot(data=cluster1_long, aes(x=date, y=value, colour=variable)) + 
  geom_line() + xlab("Date") + ylab("Value") + ggtitle("Cluster 1 Line Graph")



cluster2 <- Combi_eng_df[,c(1,13,3)]

cluster2_long <- melt(cluster2, id="date")  # convert to long format

colnames(cluster2_long)

ggplot(data=cluster2_long, aes(x=date, y=value, colour=variable)) + 
  geom_line() + xlab("Date") + ylab("Value") + ggtitle("Cluster 2 Line Graph")


cluster3 <- Combi_eng_df[,c(1,7,14,4,5,6,15)]

cluster3_long <- melt(cluster3, id="date")  # convert to long format

colnames(cluster3_long)

ggplot(data=cluster3_long, aes(x=date, y=value, colour=variable)) + 
  geom_line() + xlab("Date (2005 - 2019)") + ylab("Value") + ggtitle("Cluster 3 Line Graph")

cluster3_1 <- Combi_eng_df[,c(1,4,5,6)]

cluster3_1_long <- melt(cluster3_1, id="date")  # convert to long format

colnames(cluster3_1_long)

ggplot(data=cluster3_1_long, aes(x=date, y=value, colour=variable)) + 
  geom_line() + xlab("Date (2005 - 2019)") + ylab("Value") + ggtitle("Cluster 3.1 Line Graph")

cluster3_2 <- Combi_eng_df[,c(1,7,14,15)]

cluster3_2_long <- melt(cluster3_2, id="date")  # convert to long format

colnames(cluster3_2_long)

ggplot(data=cluster3_2_long, aes(x=date, y=value, colour=variable)) + 
  geom_line() + xlab("Date (2005 - 2019)") + ylab("Value") + ggtitle("Cluster 3.2 Line Graph")



cluster4 <- Combi_eng_df[,c(1,8,9,10)]

cluster4_long <- melt(cluster4, id="date")  # convert to long format

colnames(cluster4_long)

ggplot(data=cluster4_long, aes(x=date, y=value, colour=variable)) + 
  geom_line() + xlab("Date (2005 - 2019)") + ylab("Value") + ggtitle("Cluster 4 Line Graph")

create_report(Combi_eng_df)


cluster_asx <- Combi_eng_df[,c(1,2,12)]

cluster_asx_long <- melt(cluster_asx, id="date")  # convert to long format

colnames(cluster_asx_long)

ggplot(data=cluster_asx_long, aes(x=date, y=value, colour=variable)) + 
  geom_line() + xlab("Date") + ylab("Value") + ggtitle("ASX and DJIA correlation")



cluster_asx_rba <- Combi_eng_df[,c(1,2,8)]

cluster_asx_rba_long <- melt(cluster_asx_rba, id="date")  # convert to long format

colnames(cluster_asx_rba_long)

ggplot(data=cluster_asx_rba_long, aes(x=date, y=value, colour=variable)) +
  geom_line() + xlab("Date") + ylab("Value") + ggtitle("ASX and RBA Cash Rate correlation")


cluster_asx_yinfla <- Combi_eng_df[,c(1,2,9)]

cluster_asx_yinfla_long <- melt(cluster_asx_yinfla, id="date")  # convert to long format

colnames(cluster_asx_yinfla_long)

ggplot(data=cluster_asx_yinfla_long, aes(x=date, y=value, colour=variable)) +
  geom_line() + xlab("Date") + ylab("Value") + ggtitle("ASX and Yearly Inflation correlation")


cluster_div_pe <- Combi_eng_df[,c(1,14,13)]

cluster_div_pe_long <- melt(cluster_div_pe, id="date")  # convert to long format

colnames(cluster_div_pe_long)

ggplot(data=cluster_div_pe_long, aes(x=date, y=value, colour=variable)) +
  geom_line() + xlab("Date") + ylab("Value") + ggtitle("Dividend and P/E ratio correlation")




ls("package::ggplot2")

?create_report(Combi_eng_df)



### show binary only
binary_only <- Combi_eng_df[,c(1,2)]

binary_only_long <- melt(binary_only, id="date")  # convert to long format

colnames(binary_only_long)

ggplot(data=binary_only_long,
       aes(x=date, y=value, colour=variable)) +
  geom_line()


#### violing graph

install.packages("ExPanDaR")
library(ExPanDaR)

ls("package:ExPanDaR")

Combi_eng_df_long <- melt(Combi_eng_df, id="date")

# violin graph
ret <- prepare_by_group_violin_graph(Combi_eng_df_long, by_var = "variable", var = "value",order_by_mean = TRUE)
ret

# bar graph
ret <- prepare_by_group_bar_graph(Combi_eng_df_long, by_var = "variable", var = "value", stat_fun = mean, order_by_stat = TRUE)
ret$plot

?ExPanD

### source https://joachim-gassen.github.io/2018/10/using-the-expandar-package-for-panel-data-exploration/


####### K folds ######

cluster = makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)





## ---- KFOLD --------##
## template for installing and loading multiple packages at once
for (package in c("tidyverse","here","skimr","caret","janitor","magrittr","dplyr","reshape","moments","rsdmx","zoo","xts","Quandl","raustats","tidyquant","hydroTSM","openair","lubridate","matrixStats","psycho","Amelia","corrplot","GPArotation","aTSA")) {
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

#### logistic regression -------

# shift dataframe function
shift <- function(x, n){
  c(x[-(seq(n))], rep(NA, n))
}

# Move all variables back one column
Shift_Combi <- Combi_eng
for (j in 1:ncol(Shift_Combi)) {
  Shift_Combi[,j] <- shift(Shift_Combi[,j],1)
  
}

Shift_Combi[,1] <- Combi_eng[,1]
# Take off last column
Shift_Combi <- Shift_Combi[-nrow(Shift_Combi),]

#take a copy for 4 clust series
Shift_Combi_4clust <- Shift_Combi

#### Add in Vincent's k folds work

#-------------------------------------------------------------------------------
# Cross validation (customized)

###### on all of the variables

library(plyr)   # progress bar
library(caret)  # confusion matrix

# False positive rate
fpr <- NULL

# False negative rate
fnr <- NULL

# Miss classification
misclss <- NULL

# Number of iterations
k <- 50

# Initialize progress bar
pbar <- create_progress_bar('text')
pbar$init(k)

# Accuracy
acc <- NULL
auc_res <- NULL

set.seed(123)

for(i in 1:k){
  # Train-test splitting
  # 95% of samples -> fitting
  # 5% of samples -> testing
  smp_size <- floor(0.80 * nrow(Shift_Combi))
  index <- sample(seq_len(nrow(Shift_Combi)), size=smp_size)
  train <- Shift_Combi[index, ]
  test <- Shift_Combi[-index, ]
  
  # Fitting
  model <- glm(binary_asx~., family=binomial, data=train)
  
  # Predict results
  results_prob <- predict(model, test, type='response')
  
  # If prob > 0.5 then 1, else 0
  results <- ifelse(results_prob > 0.5,1,0)
  
  # Actual answers
  answers <- test$binary_asx
  
  # Accuracy calculation
  misClasificError <- mean(answers != results)
  
  # Collecting results
  acc[i] <- 1-misClasificError
  
  # Confusion matrix
  u <- union(results, answers)
  t <- table(factor(results, u), factor(answers, u))
  cm <- confusionMatrix(t)
  
  #cm <- confusionMatrix(table(results, answers)) 
  #cm <- confusionMatrix(data=results, reference=answers)
  #fpr[i] <- cm$table[2]/(nrow(combi_ml)-smp_size)
  #fnr[i] <- cm$table[3]/(nrow(combi_ml)-smp_size)
  fpr[i] <- cm$table[2]/(cm$table[1] + cm$table[2])
  fnr[i] <- cm$table[3]/(cm$table[3] + cm$table[4])
  misclss[i] <- (cm$table[2] + cm$table[3]) / (nrow(Shift_Combi)-smp_size)
  
  roc_obj <- roc(test$binary_asx,results, plot= TRUE)
  auc_res[i] <- auc(roc_obj)
  
  
  pbar$step()
}


# Average accuracy of the model
mean(acc)

mean(auc_res)

par(mfcol=c(1,2))

# Histogram of accuracy
hist(acc,xlab='Accuracy',ylab='Freq',
     col='cyan',border='blue',density=30)

# Boxplot of accuracy
boxplot(acc,col='cyan',border='blue',horizontal=T,xlab='Accuracy',
        main='Accuracy CV')

# Confusion matrix and plots of fpr and fnr
mean(fpr)
mean(fnr)
hist(fpr,xlab='% of fnr',ylab='Freq',main='FPR',
     col='cyan',border='blue',density=30)
hist(fnr,xlab='% of fnr',ylab='Freq',main='FNR',
     col='cyan',border='blue',density=30)


#- Accuracy of models


# Average accuracy of the model
mean(acc)



#- Histogram / Boxplot of accuracy



# Histogram of accuracy
hist(acc,xlab='Accuracy',ylab='Freq',
     col='cyan',border='blue',density=30)

# Boxplot of accuracy
boxplot(acc,col='cyan',border='blue',horizontal=T,xlab='Accuracy',
        main='Accuracy CV')



#- Mean of False postive rate & False negative rate


# Confusion matrix and plots of fpr and fnr
mean(fpr)
mean(fnr)


#- Histogram of False postive rate & False negative rate & Miss class


hist(fpr,xlab='% of fnr',ylab='Freq',main='FPR',
     col='cyan',border='blue',density=30)

hist(fnr,xlab='% of fnr',ylab='Freq',main='FNR',
     col='cyan',border='blue',density=30)

hist(fnr,xlab='% of misclassfication',ylab='Freq',main='Miss class',
     col='cyan',border='blue',density=30)

## AUC k Folds

prediction <- rev(seq_along(results))

library(pROC)
roc_obj <- roc(results, prediction)
auc(roc_obj)


##############



##### on the four cluster variables

#take asx_binary, oecd_li, quarterly_inflation, dividend yield
Shift_Combi_4clust <- Shift_Combi_4clust[,c(1,2,9,11,13)] 



# False positive rate
fpr <- NULL

# False negative rate
fnr <- NULL

# Miss classification
misclss <- NULL

# Number of iterations
k <- 50

# Initialize progress bar
pbar <- create_progress_bar('text')
pbar$init(k)

# Accuracy
acc <- NULL
auc_res <- NULL

set.seed(123)

for(i in 1:k){
  # Train-test splitting
  # 95% of samples -> fitting
  # 5% of samples -> testing
  smp_size <- floor(0.80 * nrow(Shift_Combi_4clust))
  index <- sample(seq_len(nrow(Shift_Combi_4clust)), size=smp_size)
  train <- Shift_Combi_4clust[index, ]
  test <- Shift_Combi_4clust[-index, ]
  
  # Fitting
  model <- glm(binary_asx~., family=binomial, data=train)
  
  # Predict results
  results_prob <- predict(model, test, type='response')
  
  # If prob > 0.5 then 1, else 0
  results <- ifelse(results_prob > 0.5,1,0)
  
  # Actual answers
  answers <- test$binary_asx
  
  # Accuracy calculation
  misClasificError <- mean(answers != results)
  
  # Collecting results
  acc[i] <- 1-misClasificError
  
  # Confusion matrix
  
  u <- union(results, answers)
  t <- table(factor(results, u), factor(answers, u))
  cm <- confusionMatrix(t)
  # cm <- confusionMatrix(table(results, answers)) 
  #cm <- confusionMatrix(data=results, reference=answers)
  #fpr[i] <- cm$table[2]/(nrow(combi_ml)-smp_size)
  #fnr[i] <- cm$table[3]/(nrow(combi_ml)-smp_size)
  fpr[i] <- cm$table[2]/(cm$table[1] + cm$table[2])
  fnr[i] <- cm$table[3]/(cm$table[3] + cm$table[4])
  misclss[i] <- (cm$table[2] + cm$table[3]) / (nrow(Shift_Combi_4clust)-smp_size)
  
  roc_obj <- roc(test$binary_asx,results, plot =TRUE)
  auc_res[i] <- auc(roc_obj)
  
  
  pbar$step()
}


# Average accuracy of the model
mean(acc)

mean(auc_res)


par(mfcol=c(5,5))

# Histogram of accuracy
hist(acc,xlab='Accuracy',ylab='Freq',
     col='cyan',border='blue',density=30)

# Boxplot of accuracy
boxplot(acc,col='cyan',border='blue',horizontal=T,xlab='Accuracy',
        main='Accuracy CV')

# Confusion matrix and plots of fpr and fnr
mean(fpr)
mean(fnr)
hist(fpr,xlab='% of fnr',ylab='Freq',main='FPR',
     col='cyan',border='blue',density=30)
hist(fnr,xlab='% of fnr',ylab='Freq',main='FNR',
     col='cyan',border='blue',density=30)


#- Accuracy of models


# Average accuracy of the model
mean(acc)



#- Histogram / Boxplot of accuracy



# Histogram of accuracy
hist(acc,xlab='Accuracy',ylab='Freq',
     col='cyan',border='blue',density=30)

# Boxplot of accuracy
boxplot(acc,col='cyan',border='blue',horizontal=T,xlab='Accuracy',
        main='Accuracy CV')



#- Mean of False postive rate & False negative rate


# Confusion matrix and plots of fpr and fnr
mean(fpr)
mean(fnr)


#- Histogram of False postive rate & False negative rate & Miss class


hist(fpr,xlab='% of fnr',ylab='Freq',main='FPR',
     col='cyan',border='blue',density=30)

hist(fnr,xlab='% of fnr',ylab='Freq',main='FNR',
     col='cyan',border='blue',density=30)

hist(fnr,xlab='% of misclassfication',ylab='Freq',main='Miss class',
     col='cyan',border='blue',density=30)








