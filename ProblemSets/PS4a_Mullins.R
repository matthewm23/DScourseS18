#Part 1 in R

##upload library
library("jsonlite")

##pull in data from web
system('wget -O nflstats.json "http://api.fantasy.nfl.com/v1/players/stats?statType=seasonStats&season=2010&week=1&format=json"')

##print file
system('cat nflstats.json')

##convert file to data frame
mydf<-fromJSON("nflstats.json")

##check object
class(mydf$players)

##list rows in the data frame
head(mydf$players)
