library(rvest)
library(dplyr)
library(stringr)
library(readr)
library(lubridate)
library(jsonlite)
library(httr)

#load NBA 2017-2018 data from web
webpage = read_html("https://www.basketball-reference.com/friv/standings.fcgi")
webpage

#collect useful information from web page
results = webpage %>% html_nodes(".right , #standings_w a , #team_clone a")
print(results)

#return a list of nodes (start 9 through 271)
first_result = results[1]
xml_contents(first_result)

#parse the data by extracting each team's current standing

totals = matrix(nrow = 16, ncol = 9, byrow = TRUE)

for (i in 129:270) {
  data = str_c(xml_contents(results[i]))
  totals[[i]] = matrix(data = data)
}

totals
matrixT = matrix(totals, nrow = 30, ncol = 9, byrow = TRUE)

#clean up matrix 
matrixT = matrixT[-1:-15,-1 ]
#show new matrix
print(matrixT)



## cool graphs

if (!require("ggplot2")) install.packages("ggplot2")

df = as.data.frame(matrixT)
colnames(df) = c("Teams", "Wins", "Losses", "Ratio", "GB", "PW", "PL", "PSG")
print(df)
dput(df)
quartz() 
#window()

g1 = qplot(x = Teams, y = Wins, data = df, geom = "point", colour = Wins) +
  theme(text = element_text(size = 8), 
    axis.text.x = element_text(angle = 90, hjust = 1)) +
     ggtitle("Western Conference Total Wins 2018") +
      theme_minimal() +
  geom_text(aes(label = Wins), check_overlap = TRUE, hjust= "top") 
   
    print(g1)

g2 = ggplot(df, aes(x = Wins, y = PW))
      g2 = g2 + geom_point( size = 2, shape = 23, col = heat.colors(15))
      g2 = g2 + geom_smooth(method = "lm", se = FALSE, col = "Red")
      g2 = g2 + geom_rug()
      g2 = g2 + ggtitle(" Actual Wins compared to Projected Wins")
      g2 = g2 + theme_minimal()
      g2 = g2 + theme(axis.text.y = element_text(face = "bold", colour = "green", size = 14, angle = 45),
                      axis.text.x = element_text(face = "bold", colour = "violet", size = 14, angle = 45))
      print(g2)


g3 = ggplot(df, aes(x = Teams, y = PSG))
      g3 = g3 + geom_bar(stat = "identity") +
        theme(text = element_text(size = 8), 
              axis.text.x = element_text(angle = 90, hjust = 1))
      g3 = g3 + geom_bar(aes(color = Teams), stat = "identity", fill = "white") +
        theme_minimal()
      g3 = g3 + ggtitle("Points Scored Per Game")
      g3 = g3 + scale_y_discrete( name = "PPG", breaks = c() )
      g3 = g3 + coord_flip()
      g3 = g3 + geom_text(aes(label = PSG), position = position_dodge(1.5))
      print(g3)       
        
