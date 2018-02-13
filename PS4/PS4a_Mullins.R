#R Task 1

##upload library
library("jsonlite")

##upload data from internet
system('wget -O nflstats.json "http://api.fantasy.nfl.com/v1/players/stats?statType=seasonStats&season=2010&week=1&format=json"')

##display data
system('cat nflstats.json')

##create data frame
mydf<-fromJSON("nflstats.json")

##check object type for players row
class(mydf$players)

##display players rows
class(mydf$players)


