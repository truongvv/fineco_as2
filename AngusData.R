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

# get some data ------

(url <- "https://stats.oecd.org/restsdmx/sdmx.ashx/GetData/MEI_CLI/LOLITONO.AUS.M/all?startTime=2005-01&endTime=2019-07")

dataset <- readSDMX(url)
OECDLI <- as.data.frame(dataset)

(url <- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/MERCH_IMP/-.-1.-1.-.M/all?startTime=2005-01&endTime=2019-06")

dataset <- readSDMX(url)
AusImport <- as.data.frame(dataset)

(url <- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/MERCH_EXP/-.-1.-1.-.M/all?startTime=2005-01&endTime=2019-06")

dataset <- readSDMX(url)
AusExport <- as.data.frame(dataset)

# Adjust Data ----
OECDLI <- dplyr::select(OECDLI, c(obsTime, obsValue))

