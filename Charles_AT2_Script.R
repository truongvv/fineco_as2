
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
library(MASS)
library(raustats)
library(tidyquant)
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
### do the same with inflation datasets #######
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

# complete missing month
rba_infla_day <- rba_infla %>% complete(date = seq.Date(min(date), max(date), by="day"))
rba_infla_month <- rba_infla %>% complete(date = seq.Date(min(date), max(date), by="month"))
rba_infla_week <- rba_infla %>% complete(date = seq.Date(min(date), max(date), by="week"))

# check to see confirm more rows created
nrow(rba_infla)

# take only data from the last reading before 2005 onwards
rba_infla <- subset(rba_infla, date >= '2004-03-31')
rba_infla_month <- subset(rba_infla_month, date >= '2005-01-01')

#check no rows
nrow(rba_infla)

# adding inflation rate into first two months of 2005 from last reading which is 2.5
rba_infla[1:89,2] = 2.5

# populate the rest of the NA
rba_infla <- rba_infla %>% fill('value')

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

