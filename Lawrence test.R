library(tidyverse)
library(lubridate)

# Read csv
asx200 <- read_csv("2005_2019_asx_DJIA_PE_Yield_Iron_Oil.csv")

# Explore asx200
head(asx200)

## Use lubridate to break the "date" variable into 3 columns and then use 2 columns
## to form a monthly primary key to merge with other monthly datasets
asx200 %>%
  separate(date, into=c("day","month","year"), sep="/") %>%
  unite(month.end, year, month, sep="")

head(asx200)