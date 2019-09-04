library(plyr)

# Read csv
exchange_rate <- read.csv("data/exchange_rate.csv")
asx200 <- read.csv("data/asx200_xjo.csv")

# View csv
head(exchange_rate)
head(asx200)

# Format date
exchange_rate$Date <- as.Date(exchange_rate$Date, format="%d-%b-%y")
asx200$Date <- as.Date(asx200$Date, format = "%m/%d/%y")

# View format
head(exchange_rate)
head(asx200)

# Combine data
data_combine <- join(asx200, exchange_rate, by = 'Date', type = "inner", match = "all")

# View data
head(data_combine)