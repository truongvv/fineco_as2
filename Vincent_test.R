library(plyr)
library(readxl)
library(dplyr)


# Exchange rate daily
{
  
  # Get dataframe combine
  df_combi = as.data.frame(Combi)
  df_combi['Date'] <- as.Date(rownames(df_combi), "%Y-%m-%d")
  
  # Source: https://www.rba.gov.au/statistics/historical-data.html
  read_exchange_rate <- function(file, exchange_rate_all) {
    exchange_rate <- read_xls(file, skip = 10)
    names(exchange_rate)
    exchange_rate <- exchange_rate[, 0:2]
    names(exchange_rate)[1] <- 'Date'
    names(exchange_rate)[2] <- 'Aud_usd'
    names(exchange_rate)
    exchange_rate$Date <- as.Date(exchange_rate$Date, "%Y-%m-%d", tz = "Australia/Sydney")
    exchange_rate$Aud_usd = as.numeric(exchange_rate$Aud_usd)
    
    exchange_rate_all <- rbind(exchange_rate_all, exchange_rate)
    return(exchange_rate_all)
  }
  
  # read all files
  exchange_rate_all <- NULL
  exchange_rate_all <- read_exchange_rate("data/2003-2006.xls", exchange_rate_all)
  exchange_rate_all <- read_exchange_rate("data/2007-2009.xls", exchange_rate_all)
  exchange_rate_all <- read_exchange_rate("data/2010-2013.xls", exchange_rate_all)
  exchange_rate_all <- read_exchange_rate("data/2014-2017.xls", exchange_rate_all)
  exchange_rate_all <- read_exchange_rate("data/2018-current.xls", exchange_rate_all)
  
  # Convert to data frame
  df_exchange_rate <- as.data.frame(exchange_rate_all)
  df_exchange_rate[order(df_exchange_rate$Date),]
  
  # combine data frame
  df_combi <- df_combi %>% merge(df_exchange_rate, by = 'Date', all.x = TRUE)
  df_combi[order(df_combi$Date),]
  
  # convert it back to Combi
  rownames(df_combi) <- df_combi$Date
  df_combi <- df_combi %>% select(-matches("Date"))
  Combi <- as.xts(df_combi)
}


# 
# 
# read_exchange_rate <- function(file, exchange_rate_all) {
#   exchange_rate <- read_xls(file, skip = 10)
#   names(exchange_rate)
#   exchange_rate <- exchange_rate[, 0:2]
#   names(exchange_rate)[1] <- 'Date'
#   names(exchange_rate)[2] <- 'Aud_usd'
#   names(exchange_rate)
#   exchange_rate$Date <- as.Date(exchange_rate$Date, "%Y-%m-%d", tz = "Australia/Sydney")
#   exchange_rate$Aud_usd = as.numeric(exchange_rate$Aud_usd)
#   
#   exchange_rate_all <- rbind(exchange_rate_all, exchange_rate)
#   return(exchange_rate_all)
# }
# 
# exchange_rate_all <- read_exchange_rate("data/2007-2009.xls", exchange_rate_all)
# exchange_rate_all <- read_exchange_rate("data/2010-2013.xls", exchange_rate_all)
# exchange_rate_all <- read_exchange_rate("data/2014-2017.xls", exchange_rate_all)
# exchange_rate_all <- read_exchange_rate("data/2018-current.xls", exchange_rate_all)
# 
# df_exchange_rate <- as.data.frame(exchange_rate_all)
# 
# typeof(exchange_rate_all)
# 
# head(CombiFrame)
# head(exchange_rate_all)
# 
# data_combine <- join(exchange_rate_all, CombiFrame, by = 'Date', type = "inner", match = "all")
# 
# sum(is.na(data_combine['Aud_usd']))

# df$Z <- as.POSIXct(df$Z,format="%H:%M:%S")

# Read csv
# exchange_rate <- read.csv("data/exchange_rate.csv")
# asx200 <- read.csv("data/asx200_xjo.csv")

# View csv
# head(exchange_rate)
# head(asx200)

# Format date
# exchange_rate$Date <- as.Date(exchange_rate$Date, format="%d-%b-%y")
# asx200$Date <- as.Date(asx200$Date, format = "%m/%d/%y")

# View format
# head(exchange_rate)
# head(asx200)

# Combine data
# data_combine <- join(asx200, exchange_rate, by = 'Date', type = "inner", match = "all")

# View data
# head(data_combine)