#SparkR Task

## create data frame
df1<-(iris)

##create Spark data frame
df<-createDataFrame(df1)

##verify data frames
class(df1)
class(df)

##list the first 6th rows
head(select(df, df$Sepal_Length, df$Species))

##filter rows by length
head(filter(df, df$Sepal_Length>5.5))

##combine previous two statements
head(select(filter(df, df$Sepal_Length>5.5), df$Sepal_Length, df$Species))

##compute average length and number of observations
head(summarize(groupBy(df, df$Species), mean=mean(df$Sepal_Length), count=n(df$Sepal_Length)))


##sort by variables
##omitted stats 9a.b
