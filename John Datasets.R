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
View(Combi)
nrow(Combi)


library(Quandl)

gold_forward_offer_rates <- Quandl("LBMA/GOFO", api_key="kf3rSrKM5xnKDzHNL74d")
#Gold forward rates (GOFO), in percentages; London Bullion Market Association (LBMA). LIBOR difference included. The Gold Forward Offered Rate is an international standard rate at which dealers will lend gold on a swap basis against US dollars, providing the foundation for the pricing of gold swaps, forwards and leases.
summary(gold_forward_offer_rates)
str(gold_forward_offer_rates)
View(gold_forward_offer_rates)

#Sort dates in xts
date <- seq(as.Date("2005-01-01/2019-06-01"), by = "1 month", 
           length.out = nrow(gold_forward_offer_rates))
gold_forward_offer_rates <- xts(gold_forward_offer_rates[,-1], order.by = date, frequency = 1) 
gold_forward_offer_rates <- gold_forward_offer_rates["2005-01-01/2019-06-01"]
gold_forward_offer_rates <- gold_forward_offer_rates$`GOFO - 1 Month`
Combi <- merge(Combi, gold_forward_offer_rates, join="left")
View(Combi)

gold_price_london_fixing <- Quandl("LBMA/GOLD", api_key="kf3rSrKM5xnKDzHNL74d")
#Sort dates in xts
date <- seq(as.Date("2005-01-01/2019-06-01"), by = "1 month", 
            length.out = nrow(gold_price_london_fixing))
gold_price_london_fixing <- xts(gold_price_london_fixing[,-1], order.by = date, frequency = 1) 
gold_price_london_fixing <- gold_price_london_fixing["2005-01-01/2019-06-01"]
gold_price_london_fixing <- gold_price_london_fixing$`USD (AM`
Combi <- merge(Combi, gold_price_london_fixing, join="left")
View(Combi)

#Gold Price: London Fixings, London Bullion Market Association (LBMA). Fixing levels are set per troy ounce. The London Gold Fixing Companies set the prices for gold that are globally considered as the international standard for pricing of gold. The Gold price in London is set twice a day by five LBMA Market Makers who comprise the London Gold Market Fixing Limited (LGMFL). The process starts with the announcement from the Chairman of the LGMFL to the other members of the LBMA Market Makers, then relayed to the dealing rooms where customers can express their interest as buyers or sellers and also the quantity they wish to trade. The gold fixing price is then set by collating bids and offers until the supply and demand are matched. At this point the price is announced as the 'Fixed' price for gold and all business is conducted on the basis of that price.

View(aud_usd)
aud_usd <- Quandl("PERTH/AUD_USD_D", api_key="kf3rSrKM5xnKDzHNL74d")
#Sort dates in xts
date <- seq(as.Date("2005-01-01/2019-06-01"), by = "1 month", 
            length.out = nrow(aud_usd))
aud_usd <- xts(aud_usd[,-1], order.by = date, frequency = 1) 
aud_usd <- aud_usd["2005-01-01/2019-06-01"]
aud_usd$aud_usd_bid_avg <- aud_usd$`Bid Average`
aud_usd <- aud_usd$aud_usd_bid_avg
Combi <- merge(Combi, aud_usd, join="left")
View(Combi)

View(china_imports) #yearly data...
china_imports <- Quandl("ODA/CHN_TMG_RPCH", api_key="kf3rSrKM5xnKDzHNL74d")
#Sort dates in xts
date <- seq(as.Date("2005-01-01/2019-06-01"), by = "1 month", 
            length.out = nrow(aud_usd))
aud_usd <- xts(aud_usd[,-1], order.by = date, frequency = 1) 
aud_usd <- aud_usd["2005-01-01/2019-06-01"]
aud_usd$aud_usd_bid_avg <- aud_usd$`Bid Average`
aud_usd <- aud_usd$aud_usd_bid_avg
Combi <- merge(Combi, aud_usd, join="left")
View(Combi)

#UNEMPLOYMENT
unemployment <- Quandl("FRED/NROUST", api_key="kf3rSrKM5xnKDzHNL74d")
View(unemployment)
#Sort dates in xts
date <- seq(as.Date("2005-01-01/2019-06-01"), by = "1 month", 
            length.out = nrow(unemployment))
unemployment <- xts(unemployment[,-1], order.by = date, frequency = 1) 
unemployment <- unemployment["2005-01-01/2019-06-01"]
Combi <- merge(Combi, unemployment, join="left")
View(Combi)





