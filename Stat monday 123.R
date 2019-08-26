
library(Quandl)
?Quandl
##
Quandl.api_key("f3rSrKM5xnKDzHNL74dE")

gold_forward_offer_rates <- Quandl("LBMA/GOFO", api_key="kf3rSrKM5xnKDzHNL74d")
#Gold forward rates (GOFO), in percentages; London Bullion Market Association (LBMA). LIBOR difference included. The Gold Forward Offered Rate is an international standard rate at which dealers will lend gold on a swap basis against US dollars, providing the foundation for the pricing of gold swaps, forwards and leases.
summary(gold_forward_offer_rates)

gold_price_london_fixing <- Quandl("LBMA/GOLD", api_key="kf3rSrKM5xnKDzHNL74d")
#Gold Price: London Fixings, London Bullion Market Association (LBMA). Fixing levels are set per troy ounce. The London Gold Fixing Companies set the prices for gold that are globally considered as the international standard for pricing of gold. The Gold price in London is set twice a day by five LBMA Market Makers who comprise the London Gold Market Fixing Limited (LGMFL). The process starts with the announcement from the Chairman of the LGMFL to the other members of the LBMA Market Makers, then relayed to the dealing rooms where customers can express their interest as buyers or sellers and also the quantity they wish to trade. The gold fixing price is then set by collating bids and offers until the supply and demand are matched. At this point the price is announced as the 'Fixed' price for gold and all business is conducted on the basis of that price.
summary(gold_price_london_fixing)

gold_price_aud_monthly <- Quandl("PERTH/GOLD_AUD_M", api_key="kf3rSrKM5xnKDzHNL74d")
summary(gold_price_aud_monthly)

aud_usd <- Quandl("PERTH/AUD_USD_D", api_key="kf3rSrKM5xnKDzHNL74d")
summary(gold_price_aud_monthly)

china_imports <- Quandl("ODA/CHN_TMG_RPCH", api_key="kf3rSrKM5xnKDzHNL74d")
#China Imports of Goods, % change. Units: Percent change. Estimates begin after 2018. 'Percent change of volume of imports of goods refers to the aggregate change in the quantities of imports of goods whose characteristics are unchanged. The goods and their prices are held constant, therefore changes are due to changes in quantities only. [Export and Import Price Index Manual: Theory and Practice, Glossary]' 'Source: CEIC Latest actual data: 2018 Base year: 2005 Methodology used to derive volumes: Deflation by unit value indexes (from customs data) Formula used to derive volumes: Derived from value index and unit value index Chain-weighted: No Oil coverage: Primary or unrefined products; Secondary or refined products Valuation of exports: Free on board (FOB) Valuation of imports: Cost, insurance, freight (CIF) Primary domestic currency: Chinese yuan Data last updated: 03/2019'.
summary(china_imports)
View(china_imports)


library(raustats)
cpi_all <- abs_cat_stats("6401.0")
cpi <- abs_stats("CPI", filter = list(MEASURE=1, REGION=c(1:8,50),
                                      INDEX=10001, TSEST=10, FREQUENCY="Q"))
summary(cpi)
View(cpi)
unique(cpi$region)


library(OECD)
dataset_list <- get_datasets()
View(dataset_list)
search_dataset("GDP", data = dataset_list)

View(dataset_list)


gdp <- "PPPGDP"
gdp_struc <- get_data_structure(gdp)
str(gdp_struc, max.level = 1)
View(gdp_struc)

gdp_struc$LOCATION
library(tidyverse)
gdp_struc <- as_data_frame(gdp_struc)


aus_gdp <- gdp_struc[which(gdp_struc$LOCATION==AUS)]

gdp1 <- "SNA_TABLE1"
gdp1_struc <- get_data_structure(gdp1)
str(gdp1_struc, max.level = 1)
filter_list <- list("AUS")
df <- get_dataset(dataset = gdp1, filter = filter_list)
View(df)

gdp2 <- "AEO11_OVERVIEW_CHAPTER1_TAB2_EN"
gdp2_struc <- get_data_structure(gdp2)
str(gdp2_struc, max.level = 1)
filter_list <- list("AUS")
df1 <- get_dataset(dataset = gdp2, filter = filter_list)
View(df1)

search_dataset("exports", data = dataset_list)

install.packages("comtradr")
library(comtradr)

q <- ct_search(reporters = "Australia", 
               partners = c("China"), 
               trade_direction = "exports",
               start_date = "2009-01",
               freq = "monthly", 
               commod_codes = "TOTAL")
q <- ct_use_pretty_cols(q)
View(q)
ct_commodity_db_type()
??commod_codes

ct_country_lookup(search_terms, type = c("reporter", "partner"),
                  ignore.case = TRUE,)
