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

##### EDA pre FE #####
