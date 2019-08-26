
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

## Download ABS Average monthly exchange rate
II_all <- abs_cat_stats("5654.0.40.001")

## Download the latest RBA assets and liabilities
## rba_bs <- rba_stats("A1")

## Download exchange rate '14 to '17
rbs_ex17 <- rba_stats("Exchange Rates – Daily – 2014 to 2017")

## Download exchange rate '18 to now
rbs_ex18 <- rba_stats("Exchange Rates – Daily – 2018 to Current")

## Download exchange rate '10 to now monthly
## rbs_monthly <- rba_stats("Exchange Rates – Monthly – January 2010 to latest complete month of current year")

## merge the two datasets
rbs_ex <- rbind(rbs_ex18, rbs_ex17)

## putting the datasets in order of data
ex_rate <- rbs_ex[order(rbs_ex$date),]

## check nrow
nrow(ex_rate)

## Strip ex_rate to include only FXRUSD and FXRTWI column
ex_rate1 <- ex_rate[ex_rate$series_id %in% c("FXRUSD","FXRTWI"),]

## select only the first three columns
ex_rate2 <- ex_rate1 %>% select(1:3)

## list all xts function
ls("package:tidyquant")
ls("package:xts")

# transform to monthly
## as_xts(ex_rate, date_col=date)
#Sort dates in xts
date = seq(as.Date("2005-01-01"), by = "1 month", 
           length.out = nrow(ex_rate))
ex_rate_monthly <- xts(ex_rate[,-1], order.by = date, frequency = 1)

