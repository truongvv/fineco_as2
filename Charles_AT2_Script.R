
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
library(MASS)
library(raustats)
data()

# list the data sets in all *available* packages
data(package = .packages(all.available = TRUE)) 

# abs cachelist
abs_cat_cachelist

# rba cachelist
rba_cachelist

# putting the cachelist to an array
rbalist <- rba_cachelist

## Download ABS Balance of Payments and International Investment
## II_all <- abs_cat_stats("5302.0")

## Download the latest RBA assets and liabilities
## rba_bs <- rba_stats("A1")

## Download exchange rate '14 to '17
rbs_ex17 <- rba_stats("Exchange Rates – Daily – 2014 to 2017")

## Download exchange rate '18 to now
rbs_ex18 <- rba_stats("Exchange Rates – Daily – 2018 to Current")

## merge the two datasets
rbs_ex <- rbind(rbs_ex18, rbs_ex17)

## putting the datasets in order of data
ex_rate <- rbs_ex[order(rbs_ex$date),]
