# load packages ----------

library(tidyverse)
library(here)
library(skimr)
library(janitor)
library(magrittr)
library(dplyr)
library(reshape)
library(moments)
library(rsdmx)
library(zoo)
library(xts)

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
CombiFrame <- mutate_all(CombiFrame, function(x) as.numeric(as.character(x)))


