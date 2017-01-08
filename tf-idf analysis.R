
## Trying out the td-idf analysis
install.packages("tidytext")
library(tidytext)
library(dplyr)

tweets$text <- as.character(tweets$text)

# Cleaning up the text data (there may be an easier way to do this, but at least it works):
library(tm)
tweets$proText <- tolower(tweets$text) #make it lower case
tweets$proText <- gsub('[[:punct:]]', '', tweets$proText) #remove punctuation
tweets$proText <- gsub('[[:digit:]]+', '', tweets$proText) #remove numbers
tweets$proText <- Corpus(VectorSource(tweets$proText))
tweets$proText <- tm_map(tweets$proText, removeWords, stopwords('english')) #remove stopwords
tweets$proText <- lapply(tweets$proText[1:85543], as.character)
tweets$proText <- unlist(tweets$proText)

#tf_idf:
tweets_words <- tweets %>% unnest_tokens(word, proText) %>%
  count(screenName, word, sort = TRUE) %>%
  ungroup()

tweets_words <- tweets_words  %>% bind_tf_idf(word, screenName, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))

### Top 10 tf-idf Words per Evangelical (ordered by effectiveness)

## Joel Osteen:
library(ggplot2)
a<-ggplot(tweets_words  %>% filter(screenName == "JoelOsteen") %>% group_by(screenName) %>% top_n(10), aes(x=word, y= tf_idf)) +
  geom_bar(stat="identity",fill = "grey35") +
  coord_flip() + theme_classic() +
  ggtitle("Joel Osteen") +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=2))

## Tim Tebow:
b<-ggplot(tweets_words  %>% filter(screenName == "TimTebow") %>% top_n(10), aes(x=word, y= tf_idf)) +
  geom_bar(stat="identity",fill = "grey55") +
  coord_flip() + theme_classic() +
  ggtitle("Tim Tebow") +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=2))

## Franklin Graham:
c<-ggplot(tweets_words  %>% filter(screenName == "Franklin_Graham") %>% top_n(10), aes(x=word, y= tf_idf)) +
  geom_bar(stat="identity",fill = "grey75") +
  coord_flip() + theme_classic() +
  ggtitle("Franklin Graham") +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=2))

## Joyce Meyer:
d<-ggplot(tweets_words  %>% filter(screenName == "JoyceMeyer") %>% top_n(10), aes(x=word, y= tf_idf)) +
  geom_bar(stat="identity",fill = "grey95") +
  coord_flip() + theme_classic() +
  ggtitle("JoyceMeyer") +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=2))

## Lecrae:
ggplot(tweets_words  %>% filter(screenName == "lecrae") %>% top_n(10), aes(x=word, y= tf_idf)) +
  geom_bar(stat="identity",fill = "grey35") +
  coord_flip() + theme_classic() +
  ggtitle("Top 10 tf-idf Words", subtitle = "Lecrae") +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=2))

## Tim Keller:
ggplot(tweets_words  %>% filter(screenName == "timkellernyc") %>% top_n(10), aes(x=word, y= tf_idf)) +
  geom_bar(stat="identity",fill = "grey35") +
  coord_flip() + theme_classic() +
  ggtitle("Top 10 tf-idf Words", subtitle = "Tim Keller") +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=2))

## Max Lucado:
ggplot(tweets_words  %>% filter(screenName == "MaxLucado") %>% top_n(10), aes(x=word, y= tf_idf)) +
  geom_bar(stat="identity",fill = "grey35") +
  coord_flip() + theme_classic() +
  ggtitle("Top 10 tf-idf Words", subtitle = "Max Lucado") +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=2))

## Bubba Watson:
ggplot(tweets_words  %>% filter(screenName == "bubbawatson") %>% top_n(10), aes(x=word, y= tf_idf)) +
  geom_bar(stat="identity",fill = "grey35") +
  coord_flip() + theme_classic() +
  ggtitle("Top 10 tf-idf Words", subtitle = "Bubba Watson") +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=2))

## Billy Graham:
ggplot(tweets_words  %>% filter(screenName == "BillyGraham") %>% top_n(10), aes(x=word, y= tf_idf)) +
  geom_bar(stat="identity",fill = "grey35") +
  coord_flip() + theme_classic() +
  ggtitle("Top 10 tf-idf Words", subtitle = "Billy Graham") +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=2))

## Willie Robertson:
ggplot(tweets_words  %>% filter(screenName == "williebosshog") %>% top_n(10), aes(x=word, y= tf_idf)) +
  geom_bar(stat="identity",fill = "grey35") +
  coord_flip() + theme_classic() +
  ggtitle("Top 10 tf-idf Words", subtitle = "Willie Robertson") +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=2))

## Victoria Osteen:
ggplot(tweets_words  %>% filter(screenName == "VictoriaOsteen") %>% top_n(10), aes(x=word, y= tf_idf)) +
  geom_bar(stat="identity",fill = "grey35") +
  coord_flip() + theme_classic() +
  ggtitle("Top 10 tf-idf Words", subtitle = "Victoria Osteen") +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=2))

## John Piper:
ggplot(tweets_words  %>% filter(screenName == "JohnPiper") %>% top_n(10), aes(x=word, y= tf_idf)) +
  geom_bar(stat="identity",fill = "grey35") +
  coord_flip() + theme_classic() +
  ggtitle("Top 10 tf-idf Words", subtitle = "John Piper") +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=2))

## Dave Ramsey:
ggplot(tweets_words  %>% filter(screenName == "DaveRamsey") %>% top_n(10), aes(x=word, y= tf_idf)) +
  geom_bar(stat="identity",fill = "grey35") +
  coord_flip() + theme_classic() +
  ggtitle("Top 10 tf-idf Words", subtitle = "Dave Ramsey") +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=2))

## Tony Evans:
ggplot(tweets_words  %>% filter(screenName == "drtonyevans") %>% top_n(10), aes(x=word, y= tf_idf)) +
  geom_bar(stat="identity",fill = "grey35") +
  coord_flip() + theme_classic() +
  ggtitle("Top 10 tf-idf Words", subtitle = "Tony Evans") +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=2))

## Beth Moore:
ggplot(tweets_words  %>% filter(screenName == "BethMooreLPM") %>% top_n(10), aes(x=word, y= tf_idf)) +
  geom_bar(stat="identity",fill = "grey35") +
  coord_flip() + theme_classic() +
  ggtitle("Top 10 tf-idf Words", subtitle = "Beth Moore") +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=2))

## Andy Stanley:
ggplot(tweets_words  %>% filter(screenName == "AndyStanley") %>% top_n(10), aes(x=word, y= tf_idf)) +
  geom_bar(stat="identity",fill = "grey35") +
  coord_flip() + theme_classic() +
  ggtitle("Top 10 tf-idf Words", subtitle = "Andy Stanley") +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=2))

## Mark Driscoll:
ggplot(tweets_words  %>% filter(screenName == "PastorMark") %>% top_n(10), aes(x=word, y= tf_idf)) +
  geom_bar(stat="identity",fill = "grey35") +
  coord_flip() + theme_classic() +
  ggtitle("Top 10 tf-idf Words", subtitle = "Mark Driscoll") +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=2))

## Judah Smith:
ggplot(tweets_words  %>% filter(screenName == "judahsmith") %>% top_n(10), aes(x=word, y= tf_idf)) +
  geom_bar(stat="identity",fill = "grey35") +
  coord_flip() + theme_classic() +
  ggtitle("Top 10 tf-idf Words", subtitle = "Judah Smith") +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=2))

## John Hagee:
ggplot(tweets_words  %>% filter(screenName == "PastorJohnHagee") %>% top_n(10), aes(x=word, y= tf_idf)) +
  geom_bar(stat="identity",fill = "grey35") +
  coord_flip() + theme_classic() +
  ggtitle("Top 10 tf-idf Words", subtitle = "John Hagee") +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=2))

## Louie Giglio:
ggplot(tweets_words  %>% filter(screenName == "louiegiglio") %>% top_n(10), aes(x=word, y= tf_idf)) +
  geom_bar(stat="identity",fill = "grey35") +
  coord_flip() + theme_classic() +
  ggtitle("Top 10 tf-idf Words", subtitle = "Louie Giglio") +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=2))

## Matt Chandler:
ggplot(tweets_words  %>% filter(screenName == "MattChandler74") %>% top_n(10), aes(x=word, y= tf_idf)) +
  geom_bar(stat="identity",fill = "grey35") +
  coord_flip() + theme_classic() +
  ggtitle("Top 10 tf-idf Words", subtitle = "Matt Chandler") +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=2))

## Bill Hybels:
ggplot(tweets_words  %>% filter(screenName == "BillHybels") %>% top_n(10), aes(x=word, y= tf_idf)) +
  geom_bar(stat="identity",fill = "grey35") +
  coord_flip() + theme_classic() +
  ggtitle("Top 10 tf-idf Words", subtitle = "Bill Hybels") +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=2))

## Craig Groeschel:
ggplot(tweets_words  %>% filter(screenName == "craiggroeschel") %>% top_n(10), aes(x=word, y= tf_idf)) +
  geom_bar(stat="identity",fill = "grey35") +
  coord_flip() + theme_classic() +
  ggtitle("Top 10 tf-idf Words", subtitle = "Craig Groeschel") +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=2))

## Kevin DeYoung:
ggplot(tweets_words  %>% filter(screenName == "RevKevDeYoung") %>% top_n(10), aes(x=word, y= tf_idf)) +
  geom_bar(stat="identity",fill = "grey35") +
  coord_flip() + theme_classic() +
  ggtitle("Top 10 tf-idf Words", subtitle = "Kevin DeYoung") +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=2))

## James MacDonald:
ggplot(tweets_words  %>% filter(screenName == "jamesmacdonald") %>% top_n(10), aes(x=word, y= tf_idf)) +
  geom_bar(stat="identity",fill = "grey35") +
  coord_flip() + theme_classic() +
  ggtitle("Top 10 tf-idf Words", subtitle = "James MacDonald") +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=2))

## Thomas Dexter "T.D." Jakes:
ggplot(tweets_words  %>% filter(screenName == "TDJakesShow") %>% top_n(10), aes(x=word, y= tf_idf)) +
  geom_bar(stat="identity",fill = "grey35") +
  coord_flip() + theme_classic() +
  ggtitle("Top 10 tf-idf Words", subtitle = "Thomas Dexter 'T.D.' Jakes") +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=2))

## Priscilla Shirer:
ggplot(tweets_words  %>% filter(screenName == "PriscillaShirer") %>% top_n(10), aes(x=word, y= tf_idf)) +
  geom_bar(stat="identity",fill = "grey35") +
  coord_flip() + theme_classic() +
  ggtitle("Top 10 tf-idf Words", subtitle = "Priscilla Shirer") +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=2))

## Michael Waltrip:
ggplot(tweets_words  %>% filter(screenName == "MW55") %>% top_n(10), aes(x=word, y= tf_idf)) +
  geom_bar(stat="identity",fill = "grey35") +
  coord_flip() + theme_classic() +
  ggtitle("Top 10 tf-idf Words", subtitle = "Michael Waltrip") +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=2))

## Perry Noble:
ggplot(tweets_words  %>% filter(screenName == "perrynoble") %>% top_n(10), aes(x=word, y= tf_idf)) +
  geom_bar(stat="identity",fill = "grey35") +
  coord_flip() + theme_classic() +
  ggtitle("Top 10 tf-idf Words", subtitle = "Perry Noble") +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=2))

## Roma Downey:
ggplot(tweets_words  %>% filter(screenName == "RealRomaDowney") %>% top_n(10), aes(x=word, y= tf_idf)) +
  geom_bar(stat="identity",fill = "grey35") +
  coord_flip() + theme_classic() +
  ggtitle("Top 10 tf-idf Words", subtitle = "Roma Downey") +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=2))

## Ed Stetzer:
ggplot(tweets_words  %>% filter(screenName == "edstetzer") %>% top_n(10), aes(x=word, y= tf_idf)) +
  geom_bar(stat="identity",fill = "grey35") +
  coord_flip() + theme_classic() +
  ggtitle("Top 10 tf-idf Words", subtitle = "ED Stetzer") +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=2))

## Andy Andrews:
ggplot(tweets_words  %>% filter(screenName == "AndyAndrews") %>% top_n(10), aes(x=word, y= tf_idf)) +
  geom_bar(stat="identity",fill = "grey35") +
  coord_flip() + theme_classic() +
  ggtitle("Top 10 tf-idf Words", subtitle = "Andy Andrews") +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=2))

## Pete Wilson:
ggplot(tweets_words  %>% filter(screenName == "pwilson") %>% top_n(10), aes(x=word, y= tf_idf)) +
  geom_bar(stat="identity",fill = "grey35") +
  coord_flip() + theme_classic() +
  ggtitle("Top 10 tf-idf Words", subtitle = "Pete Wilson") +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=2))

## Ron Edmondson:
ggplot(tweets_words  %>% filter(screenName == "RonEdmondson") %>% top_n(10), aes(x=word, y= tf_idf)) +
  geom_bar(stat="identity",fill = "grey35") +
  coord_flip() + theme_classic() +
  ggtitle("Top 10 tf-idf Words", subtitle = "Ron Edmondson") +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=2))

## Steve Strange:
ggplot(tweets_words  %>% filter(screenName == "sstrange") %>% top_n(10), aes(x=word, y= tf_idf)) +
  geom_bar(stat="identity",fill = "grey35") +
  coord_flip() + theme_classic() +
  ggtitle("Top 10 tf-idf Words", subtitle = "Steve Strange") +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=2))

top10<-filter(tweets,followers >= 724000)
tweets_words <- top10 %>% unnest_tokens(word, proText) %>%
  count(screenName, word, sort = TRUE) %>%
  ungroup()

top10fav<-filter(tweets,favscore >= 444.1184466)
tweets_words <- top10fav %>% unnest_tokens(word, proText) %>%
  count(screenName, word, sort = TRUE) %>%
  ungroup()

tweets_words <- tweets_words  %>% bind_tf_idf(word, screenName, n)
tweets_words <- tweets_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) 


total_words <- tweets_words %>% group_by(screenName) %>% summarize(total = sum(n))
all_words <- left_join(tweets_words, total_words)
all_words <- all_words %>% bind_tf_idf(word, screenName, n)

install.packages("ggstance")
install.packages("ggthemes")
library(ggstance)
library(ggthemes)
library(viridis)
total_words <- tweets_words %>% group_by(screenName) %>% 
  top_n(10) %>% 
  mutate(word=reorder(word,tf_idf))
Words<-total_words[with(total_words, order(tf_idf,decreasing=TRUE)),]
ggplot(tweets_words, aes(tf_idf, word, fill = screenName, alpha = tf_idf)) +
  geom_barh(stat = "identity", show.legend = FALSE) +
  ggtitle("Top tf-idf Words",subtitle = "Top 10 Evangelicals Based on Favorites Score.") +
  ylab("") + xlab("tf-idf") +
  theme_tufte(base_family = "Arial", base_size = 13, ticks = FALSE) +
  scale_alpha_continuous(range = c(0.6, 1)) +
  scale_x_continuous(expand=c(0,0)) +
  facet_wrap(~screenName, ncol = 5, scales = "free") +
  scale_fill_viridis(end = 0.85, discrete=TRUE) +
  theme(strip.text=element_text(hjust=0)) +
  theme(strip.text = element_text(face = "italic"))


## This is where it just gets terrible. Everyone is just tweeting about their own little niche. 
all_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

install.packages("ggstance")
library(ggstance)
library(ggplot2)
install.packages("viridis")
library(viridis)
install.packages("colorspace")
library(colorspace)
pal <- choose_palette()

plot1 <- all_words %>% 
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))
ggplot(plot1[1:20,], aes(tf_idf, word, fill = screenName)) +
  geom_barh(stat = "identity") +
  labs(title = "Highest tf-idf Words in Evangelical Tweets",
       y = NULL, x = "tf-idf") +
  theme_classic() +
  theme(legend.position="none") +
  scale_fill_manual(values=pal(18)) +
  scale_alpha_manual(plot1[1:20,],"tf_idf") +
  theme(legend.title=element_blank()) +
  theme(legend.justification=c(1,0), legend.position=c(1,0)) +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=2))  


aggdata$replyCat<-NA
aggdata$replyCat[aggdata$replyToSID >= 1.239e+17] <- "1st Quartile"
aggdata$replyCat[aggdata$replyToSID > 5.563e+17] <- "2nd Quartile"
aggdata$replyCat[aggdata$replyToSID > 6.276e+17] <- "3rd Quartile"
aggdata$replyCat[aggdata$replyToSID > 7.533e+17] <- "4th Quartile"

aggdata.prop<-data.frame(prop.table(table(aggdata$replyCat,
                                          aggdata$followers),1))

ggplot(aggdata.prop %>% filter(Var2 != "NA" | Var1 != "NA"),aes(Var1,Freq,fill=Var2)) +
  geom_col() + theme_classic() +
  scale_y_continuous(labels=scales::percent) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_grey() 
