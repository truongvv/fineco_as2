create_report(Combi)

install.packages("dataMeta")
library(dataMeta)

##### Building up data dictionary ####
{
# list functions vailable from a package
ls("package:dataMeta")

?build_dict
?build_linker

colnames(Combi)
nrow(Combi)
Combi_df <- as.data.frame(Combi)
colnames(Combi_df)

variable_desc <- c("ASX 200 Index","OECD Leading Indicators","Import Values from ABS (AUD)","Export Values from ABS (AUD)","LBMA Gold Price (USD)","Unemployment rate (%)","RBA Cash Rate target as announced (%)","Inflation rate calculated over the last 12 months (%)","Inflation rate calculated over the last 3 months (%)","USD vs AUD","Dow Jones Industrial Index","Price to Earning Ratio","Dividend Yield as percentage (%)","Iron Price (AUD) ","Oil Price (AUD)")

var_type <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

my.data <- Combi_df

link <- build_linker(my.data = my.data, variable_description = variable_desc, variable_type = var_type)

link

dictionary <- build_dict(my.data = my.data, linker = link, option_description = NULL, prompt_varopts = FALSE)
dictionary

# change column name
colnames(dictionary) <- c("Variable Name","Variable Description", "Range of values")

dictionary


install.packages("formattable")
library(formattable)

formattable(dictionary)

ls("package:formattable")

customGreen = "#71CA97"
customGreen0 = "#a3f6a4"
customGreen1 = "#16d21a"
customGreen2 = "#074608"
customRed = "#ff7f7f"
customBlue = "#111ba3"

improvement_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.weight = "bold", 
              color = ifelse(x > 0, customGreen1, ifelse(x < 0, customRed, "black"))))

formattable(dictionary, align = c("l","l","r"), list(
  `Variable Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
  `Range of values` = improvement_formatter))

?color_tile

##### POST FEATURE ENGINEERING #####

colnames(Combi_eng)

variable_desc1 <- c("Changes in ASX index 1 for up and 0 for down"," Percentage changes in OECD Leading Indicators","Percentage changes in Import Values from ABS"," Percentage changes in Export Values from ABS","Percentage changes in LBMA Gold Price"," Percentage changes in Unemployment rate","Percentage changes in RBA Cash Rate target as announced (%)","Percentage changes in Inflation rate calculated over the last 12 months","Percentage changes in Inflation rate calculated over the last 3 months","Percentage changes in USD vs AUD","Percentage changes in Dow Jones Industrial Index","Percentage changes in Price to Earning Ratio","Percentage changes in Dividend Yield as percentage","Percentage changes in Iron Price","Percentage changes in Oil Price")

my.data1 <- Combi_eng

link1 <- build_linker(my.data = my.data1, variable_description = variable_desc1, variable_type = var_type)

link1

dictionary1 <- build_dict(my.data = my.data1, linker = link1, option_description = NULL, prompt_varopts = FALSE)
dictionary1

colnames(dictionary1) <- c("Variable Name","Variable Description", "Range of values")

formattable(dictionary1, align = c("l","l","r"), list(
  `Variable Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
  `Range of values` = improvement_formatter))


##### source #####
# https://www.littlemissdata.com/blog/prettytables

}

##### EDA pre Featre Engineering #####
{
Combi_df <- data.frame(date=index(Combi), coredata(Combi))
Combi_df$date <- as.Date(Combi_df$date)
colnames(Combi_df)
#Plotting rba_cash_rate
ggplot(Combi_df, aes(date, rba_cash_rate)) + geom_line() +
  xlab("Date") + ylab("RBA Cash Rate") + ggtitle("Cash Rate Target as announced by the RBA") +
  annotate(geom="text", x=as.Date("2012-01-01"), y=6,
           label="Lehman Brothers filed for bankruptcy") +
  annotate(geom="text", x=as.Date("2011-06-01"), y=7.25,
          label="Peaked Cash Rate before GFC") +
  annotate(geom="text", x=as.Date("2006-10-01"), y=3.25,
           label="Fifth consecutive rate cut") +
  annotate(geom="text", x=as.Date("2014-12-15"), y=4.75,
           label="China's growth started to slow down") +
  annotate(geom="text", x=as.Date("2010-05-07"), y=2.5,
           label="Mining investment has slowed down") +
  annotate(geom="text", x=as.Date("2014-08-07"), y=1.5,
           label="Start of a stable rate") +
  coord_cartesian(clip = 'off') +
  annotate(geom="point", x=as.Date("2008-09-30"), y=6, size=8, shape=21, fill="transparent") +
  annotate(geom="point", x=as.Date("2008-08-01"), y=7.25, size=8, shape=21, fill="transparent") +
  annotate(geom="point", x=as.Date("2009-02-01"), y=3.25, size=8, shape=21, fill="transparent") +
  annotate(geom="point", x=as.Date("2011-10-01"), y=4.75, size=8, shape=21, fill="transparent") +
  annotate(geom="point", x=as.Date("2013-08-07"), y=2.5, size=8, shape=21, fill="transparent") +
  annotate(geom="point", x=as.Date("2016-08-07"), y=1.5, size=8, shape=21, fill="transparent") +
  geom_smooth(method='lm') +
  theme_ipsum()

#Plotting yearly inflation rate
ggplot(Combi_df, aes(date, yearly_inflation)) + geom_line() +
  xlab("Date") + ylab("Yearly Inflation") + ggtitle("Year-ended Consumer Price Inflation") +
  annotate(geom="text", x=as.Date("2011-08-30"), y=5,
           label="GFC started affecting inflation") +
  annotate(geom="text", x=as.Date("2012-02-28"), y=3.7,
           label="Low confidence affected by GFC") +
  annotate(geom="text", x=as.Date("2011-04-30"), y=1.2,
           label="Credit growth") +
  annotate(geom="text", x=as.Date("2014-08-30"), y=1,
           label="Lowest inflation rate") +
  coord_cartesian(clip = 'off') +
  annotate(geom="point", x=as.Date("2008-11-30"), y=5, size=8, shape=21, fill="transparent") +
  annotate(geom="point", x=as.Date("2009-02-28"), y=3.7, size=8, shape=21, fill="transparent") +
  annotate(geom="point", x=as.Date("2009-11-30"), y=1.2, size=8, shape=21, fill="transparent") +
  annotate(geom="point", x=as.Date("2016-08-30"), y=1, size=8, shape=21, fill="transparent") +
  geom_smooth(method='lm') +
  theme_ipsum()

Combi_df[,c(1,9,10)]

#Plotting quarterly and yearly inflation rate
colnames(Combi_df)
Infla <- Combi_df[,c(1,9,10)]
Infla_long <- melt(Infla, id="date")  # convert to long format
ggplot(data=Infla_long,
       aes(x=date, y=value, colour=variable)) +
  geom_line()

# source https://stackoverflow.com/questions/3777174/plotting-two-variables-as-lines-using-ggplot2-on-the-same-graph


}

##### EDA post Feature Engineering ####
Combi_eng_df <- data.frame(date=index(Combi_eng), coredata(Combi_eng))
Combi_eng_df$date <- as.Date(Combi_eng_df$date)
colnames(Combi_eng_df)
#Plotting rba_cash_rate
ggplot(Combi_eng_df, aes(date, rba_cash_rate)) + geom_line() +
  xlab("Date") + ylab("RBA Cash Rate") + ggtitle("Cash Rate Target as announced by the RBA") +
  #annotate(geom="text", x=as.Date("2012-01-01"), y=6,
  #        label="Lehman Brothers filed for bankruptcy") +
  # coord_cartesian(clip = 'off') +
  #annotate(geom="point", x=as.Date("2008-09-30"), y=6, size=8, shape=21, fill="transparent") +
  geom_smooth(method='lm') +
  theme_ipsum()

install.packages("reshape2")
library(reshape2)

colnames(Combi_eng_df)

cluster1 <- Combi_eng_df[,c(1,2,11,12,16)]

cluster1_long <- melt(cluster1, id="date")  # convert to long format

colnames(cluster1_long)

ggplot(data=cluster1_long,
       aes(x=date, y=value, colour=variable)) +
  geom_line()


cluster2 <- Combi_eng_df[,c(1,3,13)]

cluster2_long <- melt(cluster2, id="date")  # convert to long format

colnames(cluster2_long)

ggplot(data=cluster2_long,
       aes(x=date, y=value, colour=variable)) +
  geom_line()


cluster3 <- Combi_eng_df[,c(1,8,9,10)]

cluster3_long <- melt(cluster3, id="date")  # convert to long format

colnames(cluster3_long)

ggplot(data=cluster3_long,
       aes(x=date, y=value, colour=variable)) +
  geom_line()



cluster4 <- Combi_eng_df[,c(1,4,5,6,14,15)]

cluster4_long <- melt(cluster4, id="date")  # convert to long format

colnames(cluster4_long)

ggplot(data=cluster4_long,
       aes(x=date, y=value, colour=variable)) +
  geom_line()

create_report(Combi_eng_df)

