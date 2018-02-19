#Task 2, importing data from an API

library(rvest)
library(dplyr)
library(stringr)
library(readr)
library(lubridate)
library(jsonlite)
library(httr)

#install Quandl
install.packages("Quandl")
library(Quandl)
Quandl.api_key("MhKfscxHpswnQymJsqgr")

#upload Natural gas prices from 1984-2015
info = Quandl("BP/GAS_PRICES",start_date="1984-12-30")
info$`Natural Gas - US Henry Hub`
hHub = list(info$`Natural Gas - US Henry Hub`)
names(hHub) = c("Henry Hub Yearly Average Price")

#build data frame for henry hub prices
df = data.frame(hHub)
row.names(df) = (2016 - 1:nrow(df))

#save the newly created data set as csv
write_csv(df, "HenryHub.csv")

