library(rvest)
library(dplyr)
library(stringr)
library(readr)
library(lubridate)
library(jsonlite)
library(httr)


#load all information from the web page
webpage = read_html("https://www.reuters.com/article/us-aviation-safety/2017-safest-year-on-record-for-commercial-passenger-air-travel-groups-idUSKBN1EQ17L")
webpage

#collect useful information from web page
results = webpage %>% html_nodes("p")
results

#return a list of nodes
first_result = results[1]
xml_contents(first_result)

#build matrix of data
records = vector("list", length = length(results))

#start for loop to look to add each line to 
for( i in length(results))   {
  line = xml_contents(results)[i] %>% html_text(trim = TRUE)
  line1 = str_sub(line, 2, -2)
  records[[i]] = data_frame(line1 = line1)
}
#remove junk rows
df = records
df1 = df[-1:-3,]
df1 = df1[-11:-15,]

#finished data frame
df1

#save the newly created data set as csv
write_csv(df1, "AviationData.csv")

