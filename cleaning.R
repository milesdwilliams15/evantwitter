tweets <- read.csv("D:/evantwitter/total.csv")
dim(tweets)
library(twitteR)
library(ROAuth)
library(tm)
library(stringr)
library(wordcloud)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
library(RColorBrewer)
library(ggstance)
library(viridis)


count <- count(tweets, screenName)
count  %>% arrange(count, desc(n))
count <- filter(count, n >100)

## Which Users Tweet the Most
ggplot(count, aes(x=reorder(screenName, n), y = n)) + geom_bar(stat="identity") + coord_flip()

## Total Distribution of Tweets Across Time
ggplot(tweets, aes(created)) + geom_histogram(aes(fill = ..count..), bins = 270) + 
  xlab("Date Tweeted") + ylab("Total Number of Tweets")


## Tweets by Day of the Week
ggplot(data = tweets, aes(x = wday(created, label = TRUE))) +
  geom_histogram(breaks = seq(0.5, 7.5, by =1), aes(fill = ..count..), stat = "count") +
  theme(legend.position = "none") +
  xlab("Day of the Week") + ylab("Number of tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

## Tweets by Month
ggplot(data = tweets, aes(x = month(created, label = TRUE))) +
  geom_histogram(breaks = seq(0.5, 7.5, by =1), aes(fill = ..count..), stat = "count") +
  theme(legend.position = "none") +
  xlab("Day of the Week") + ylab("Number of tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

## Stripping out the Tweets by Hours
tweets$hours <- format(as.POSIXct(strptime(tweets$created,"%Y-%m-%d %H:%M:%S",tz="")) ,format = "%H:%M")
tweets$created <-as.POSIXct(tweets$created)


nohandles <- str_replace_all(tweets$text, "@\\w+", "")
wordCorpus <- Corpus(VectorSource(nohandles))
wordCorpus <- tm_map(wordCorpus, removePunctuation)
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))
wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("english"))
wordCorpus <- tm_map(wordCorpus, stripWhitespace)
wordCorpus <- tm_map(wordCorpus, stemDocument)

wordCorpus <- tm_map(wordCorpus, removeWords, c("amp", "jaylive", "18885675635", "'re", "periscope", "rt", "…", "1", "2", "4", "u", "3", "10", "7", "'s", "vom", "5"))

library(RColorBrewer)
pal <- brewer.pal(9,"YlGnBu")
pal <- pal[-(1:4)]
wordcloud(words = wordCorpus, scale=c(5,0.1), max.words=100, random.order=FALSE,
rot.per=0.35, use.r.layout=FALSE, colors=pal)




