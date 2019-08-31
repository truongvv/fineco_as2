
## template for installing and loading multiple packages at once
for (package in c("tidyverse","ggplot2","scales","stringr","caret","dplyr","gbm","ROCR","DataExplorer", "pls", "naniar","UpSetR","VIM","magrittr","GGally", "lubridate", "magrittr", "caret", "corrplot","hydroGOF","parallel","doParallel","xgboost","pls","randomForest","Boruta","pdp", "vip")) {
  if (!package %in% installed.packages()) {
    install.packages(package)
  }
  if (!package %in% .packages()) {
    library(package, character.only = TRUE)
  }
}

install.packages("raustats")
install.packages("tidyquant")
install.packages("hydroTSM")
library(MASS)
library(raustats)
library(tidyquant)
library(hydroTSM)
data()

# list the data sets in all *available* packages
data(package = .packages(all.available = TRUE)) 

# abs cachelist
abs_cat_cachelist

# putting the cachelist to an array
abslist <- abs_cat_cachelist

# rba cachelist
rba_cachelist

# putting the cachelist to an array
rbalist <- rba_cachelist

## Download datasets
IntInvestment <- abs_cat_stats("5302.0")
rba_mon <- rba_stats("A2")
rba_intr <- rba_stats("F13")
rba_irmon <- rba_stats("F1.1")
rba_infla <- rba_stats("G1")
rba_gbudget <- rba_stats("E1")
rba_gbond <- rba_stats(url = "https://www.rba.gov.au/statistics/tables/xls/f02hist.xls")

## useless exchange rate codes
{
## Download the latest RBA assets and liabilities
## rba_bs <- rba_stats("A1")
## usesless now as Vincent is doing the exchange rate
## Download exchange rate '14 to '17
## rbs_ex17 <- rba_stats("Exchange Rates – Daily – 2014 to 2017")
## Download exchange rate '18 to now
## rbs_ex18 <- rba_stats("Exchange Rates – Daily – 2018 to Current")
## Download exchange rate '10 to now monthly
## rbs_monthly <- rba_stats("Exchange Rates – Monthly – January 2010 to latest complete month of current year")
## merge the two datasets
## rbs_ex <- rbind(rbs_ex18, rbs_ex17)

## putting the datasets in order of data
## ex_rate <- rbs_ex[order(rbs_ex$date),]

## check nrow
## nrow(ex_rate)

## Strip ex_rate to include only FXRUSD and FXRTWI column
## ex_rate1 <- ex_rate[ex_rate$series_id %in% c("FXRUSD","FXRTWI"),]

## select only the first three columns
## ex_rate2 <- ex_rate1 %>% select(1:3)
}

## data munging
colnames(rba_mon)
unique(rba_mon$title)
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

########
### do the same with inflation datasets on year-end inflation #######
########

colnames(rba_infla)
unique(rba_infla$title)
unique(rba_infla$frequency)
rba_infla<- subset(rba_infla, title == "Year-ended inflation")
unique(rba_infla$title)

col <- c('date','value','title')
rba_infla <- rba_infla[,col]
colnames(rba_infla)

str(rba_infla)

col1 <- c('date','value')
rba_infla <- rba_infla[,col1]

nrow(rba_infla)
nrow(rba_infla_month)

# complete missing month and put it on a new variable
rba_infla_day <- rba_infla %>% complete(date = seq.Date(min(date), max(date), by="day"))
# rba_infla_month <- rba_infla %>% complete(date = seq.Date(min(date), max(date), by="month"))
# rba_infla_week <- rba_infla %>% complete(date = seq.Date(min(date), max(date), by="week"))

# check to see confirm more rows created
nrow(rba_infla_day)

# populate the rest of the NA
rba_infla_day <- rba_infla_day %>% fill('value')

# check to confirm no na
unique(is.na(rba_infla_day))

# take only data from the last reading before 2005 onwards
rba_infla_day <- subset(rba_infla_day, date >= '2005-01-01')

# take the monthly data
# x <- rba_infla_day
rba_infla_day <- as.data.frame(rba_infla_day)

rba_infla_day$date <- as.POSIXct.Date(rba_infla_day$date)
rba_infla_day$date <- strptime(rba_infla_day$date,"%Y-%m-%d")
# x<-as.xts(x)
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

########
### do the same with inflation datasets on quarterly-end inflation #######
########

rba_infla_qrt <- rba_stats("G1")

{
  colnames(rba_infla_qrt)
  unique(rba_infla_qrt$title)
  unique(rba_infla_qrt$frequency)
  rba_infla_qrt<- subset(rba_infla_qrt, title == "Quarterly inflation")
  unique(rba_infla_qrt$title)
  
  col <- c('date','value','title')
  rba_infla_qrt <- rba_infla_qrt[,col]
  colnames(rba_infla_qrt)
  
  str(rba_infla)
  
  col1 <- c('date','value')
  rba_infla_qrt <- rba_infla_qrt[,col1]
  
  nrow(rba_infla_qrt)
  
  # complete missing month and put it on a new variable
  rba_infla_qrt_day <- rba_infla_qrt %>% complete(date = seq.Date(min(date), max(date), by="day"))
  # rba_infla_month <- rba_infla %>% complete(date = seq.Date(min(date), max(date), by="month"))
  # rba_infla_week <- rba_infla %>% complete(date = seq.Date(min(date), max(date), by="week"))
  
  # check to see confirm more rows created
  nrow(rba_infla_qrt_day)
  
  # populate the rest of the NA
  rba_infla_qrt_day <- rba_infla_qrt_day %>% fill('value')
  
  #confirm no NA
  unique(is.na(rba_infla_day))
  
  # take only data from the last reading before 2005 onwards
  rba_infla_qrt_day <- subset(rba_infla_qrt_day, date >= '2005-01-01')
  
  # take the monthly data
  # x <- rba_infla_day
  # convert to data frame
  rba_infla_qrt_day <- as.data.frame(rba_infla_qrt_day)
  
  rba_infla_qrt_day$date <- as.POSIXct.Date(rba_infla_qrt_day$date)
  rba_infla_qrt_day$date <- strptime(rba_infla_qrt_day$date,"%Y-%m-%d")
  # x<-as.xts(x)
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
  # rba_infla_qrt_mon$V1<- format(rba_infla_qrt_mon$V1, digits=1, nsmall=1)
  
  
  head(rba_infla_qrt_mon)
  tail(rba_infla_qrt_mon)
  nrow(rba_infla_qrt_mon)
  
  colnames(rba_infla_qrt_mon) <- c("Quarterly Inflation")
  colnames(rba_infla_qrt_mon)
  
  
}



#ls("package:xts")

# y <- xts(rba_infla_day[,-1], order.by=rba_infla_day[,1])
# z <- daily2monthly(y,FUN=mean, nar.rm=FALSE)
nrow(z)

# take only data from the last reading before 2005 onwards
rba_infla <- subset(rba_infla, date >= '2004-03-31')
rba_infla_month <- subset(rba_infla_month, date >= '2005-01-01')

#check no rows
nrow(rba_infla)

# adding inflation rate into first two months of 2005 from last reading which is 2.5
rba_infla[1:89,2] = 2.5



date = seq(as.Date("2005-01-01"), by = "1 month", 
           length.out = nrow(rba_infla))
rba_infla <- xts(rba_infla[,-1], order.by = date, frequency = 1)


## list all xts function
ls("package:tidyquant")
ls("package:xts")

# transform to monthly
## as_xts(ex_rate, date_col=date)
#Sort dates in xts
date = seq(as.Date("2005-01-01"), by = "1 month", 
           length.out = nrow(ex_rate))
ex_rate_monthly <- xts(ex_rate[,-1], order.by = date, frequency = 1)

