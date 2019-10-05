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

Combi_eng_dict <- Combi_eng[,c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)]

colnames(Combi_eng)
colnames(Combi_eng_dict)


variable_desc1 <- c("Changes in ASX index 1 for up and 0 for down"," Z - Score in OECD Leading Indicators","Z - Score in Import Values from ABS"," Z - Score in Export Values from ABS","Z - Score in LBMA Gold Price"," Z - Score in Unemployment rate","Z - Score in RBA Cash Rate target as announced (%)","Z - Score in Inflation rate calculated over the last 12 months","Z - Score in Inflation rate calculated over the last 3 months","Z - Score in USD vs AUD","Z - Score in Dow Jones Industrial Index","Z - Score in Price to Earning Ratio","Z - Score in Dividend Yield as percentage","Z - Score in Iron Price","Z - Score in Oil Price")


my.data1 <- Combi_eng_dict

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

Combi_eng$date <- seq(as.Date("2005-01-01"), by = "month", length.out = 174)

ncol(Combi_eng)
colnames(Combi_eng)

Combi_eng <- Combi_eng[,c(16,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)]

Combi_eng_df <- Combi_eng

Combi_eng_df <- data.frame(date=index(Combi_eng), coredata(Combi_eng))
Combi_eng_df$date <- as.Date(Combi_eng_df$date)
colnames(Combi_eng_df)

install.packages("reshape2")
library(reshape2)

colnames(Combi_eng_df)

cluster1 <- Combi_eng_df[,c(1,12,11,16)]

cluster1_long <- melt(cluster1, id="date")  # convert to long format

colnames(cluster1_long)

ggplot(data=cluster1_long, aes(x=date, y=value, colour=variable)) + 
  geom_line() + xlab("Date") + ylab("Value") + ggtitle("Cluster 1 Line Graph")
  


cluster2 <- Combi_eng_df[,c(1,13,3)]

cluster2_long <- melt(cluster2, id="date")  # convert to long format

colnames(cluster2_long)

ggplot(data=cluster2_long, aes(x=date, y=value, colour=variable)) + 
  geom_line() + xlab("Date") + ylab("Value") + ggtitle("Cluster 2 Line Graph")


cluster3 <- Combi_eng_df[,c(1,7,14,4,5,6,15)]

cluster3_long <- melt(cluster3, id="date")  # convert to long format

colnames(cluster3_long)

ggplot(data=cluster3_long, aes(x=date, y=value, colour=variable)) + 
  geom_line() + xlab("Date (2005 - 2019)") + ylab("Value") + ggtitle("Cluster 3 Line Graph")

cluster3_1 <- Combi_eng_df[,c(1,4,5,6)]

cluster3_1_long <- melt(cluster3_1, id="date")  # convert to long format

colnames(cluster3_1_long)

ggplot(data=cluster3_1_long, aes(x=date, y=value, colour=variable)) + 
  geom_line() + xlab("Date (2005 - 2019)") + ylab("Value") + ggtitle("Cluster 3.1 Line Graph")

cluster3_2 <- Combi_eng_df[,c(1,7,14,15)]

cluster3_2_long <- melt(cluster3_2, id="date")  # convert to long format

colnames(cluster3_2_long)

ggplot(data=cluster3_2_long, aes(x=date, y=value, colour=variable)) + 
  geom_line() + xlab("Date (2005 - 2019)") + ylab("Value") + ggtitle("Cluster 3.2 Line Graph")



cluster4 <- Combi_eng_df[,c(1,8,9,10)]

cluster4_long <- melt(cluster4, id="date")  # convert to long format

colnames(cluster4_long)

ggplot(data=cluster4_long, aes(x=date, y=value, colour=variable)) + 
  geom_line() + xlab("Date (2005 - 2019)") + ylab("Value") + ggtitle("Cluster 4 Line Graph")

create_report(Combi_eng_df)


cluster_asx <- Combi_eng_df[,c(1,2,12)]

cluster_asx_long <- melt(cluster_asx, id="date")  # convert to long format

colnames(cluster_asx_long)

ggplot(data=cluster_asx_long, aes(x=date, y=value, colour=variable)) + 
  geom_line() + xlab("Date") + ylab("Value") + ggtitle("ASX and DJIA correlation")



cluster_asx_rba <- Combi_eng_df[,c(1,2,8)]

cluster_asx_rba_long <- melt(cluster_asx_rba, id="date")  # convert to long format

colnames(cluster_asx_rba_long)

ggplot(data=cluster_asx_rba_long, aes(x=date, y=value, colour=variable)) +
  geom_line() + xlab("Date") + ylab("Value") + ggtitle("ASX and RBA Cash Rate correlation")


cluster_asx_yinfla <- Combi_eng_df[,c(1,2,9)]

cluster_asx_yinfla_long <- melt(cluster_asx_yinfla, id="date")  # convert to long format

colnames(cluster_asx_yinfla_long)

ggplot(data=cluster_asx_yinfla_long, aes(x=date, y=value, colour=variable)) +
  geom_line() + xlab("Date") + ylab("Value") + ggtitle("ASX and Yearly Inflation correlation")


cluster_div_pe <- Combi_eng_df[,c(1,14,13)]

cluster_div_pe_long <- melt(cluster_div_pe, id="date")  # convert to long format

colnames(cluster_div_pe_long)

ggplot(data=cluster_div_pe_long, aes(x=date, y=value, colour=variable)) +
  geom_line() + xlab("Date") + ylab("Value") + ggtitle("Dividend and P/E ratio correlation")




ls("package::ggplot2")

?create_report(Combi_eng_df)



### show binary only
binary_only <- Combi_eng_df[,c(1,2)]

binary_only_long <- melt(binary_only, id="date")  # convert to long format

colnames(binary_only_long)

ggplot(data=binary_only_long,
       aes(x=date, y=value, colour=variable)) +
  geom_line()


#### violing graph

install.packages("ExPanDaR")
library(ExPanDaR)

ls("package:ExPanDaR")

Combi_eng_df_long <- melt(Combi_eng_df, id="date")

# violin graph
ret <- prepare_by_group_violin_graph(Combi_eng_df_long, by_var = "variable", var = "value",order_by_mean = TRUE)
ret

# bar graph
ret <- prepare_by_group_bar_graph(Combi_eng_df_long, by_var = "variable", var = "value", stat_fun = mean, order_by_stat = TRUE)
ret$plot

?ExPanD

### source https://joachim-gassen.github.io/2018/10/using-the-expandar-package-for-panel-data-exploration/


####### K folds ######

cluster = makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)
