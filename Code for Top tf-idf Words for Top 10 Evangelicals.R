
## Trying out the td-idf analysis
install.packages("tidytext")
library(tidytext)
library(dplyr)
library(ggplot2)

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

#Plot top tf-idf scores for 
#top 10 evangelicals (by # of followers)
#screen names in descending order by # of followers:
a<-ggplot(tweets_words  %>% filter(screenName == "JoelOsteen") %>% group_by(screenName) %>% top_n(10), aes(x=reorder(word,tf_idf), y= tf_idf,alpha=tf_idf)) +
  geom_bar(stat="identity",fill = "grey30") +
  coord_flip() + theme_classic() + xlab("") + ylab("tf-idf") +
  scale_alpha_continuous(range = c(0.6, 1)) +
  ggtitle("Top tf-idf Words",subtitle="Joel Osteen") +
  geom_text(aes(label=word), hjust=-0.1, colour="black") +
  theme(plot.subtitle=element_text(size=10, hjust=0, face="italic", color="black")) +
  theme(legend.position="none") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ylim(c(0,.25))

b<-ggplot(tweets_words  %>% filter(screenName == "JoyceMeyer") %>% group_by(screenName) %>% top_n(10), aes(x=reorder(word,tf_idf), y= tf_idf,alpha=tf_idf)) +
  geom_bar(stat="identity",fill = "grey30") +
  coord_flip() + theme_classic() + xlab("") + ylab("tf-idf") +
  scale_alpha_continuous(range = c(0.6, 1)) +
  ggtitle("",subtitle="Joyce Meyer") +
  geom_text(aes(label=word), hjust=-0.1, colour="black") +
  theme(plot.subtitle=element_text(size=10, hjust=0, face="italic", color="black")) +
  theme(legend.position="none") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ylim(c(0,.25))

c<-ggplot(tweets_words  %>% filter(screenName == "TimTebow") %>% group_by(screenName) %>% top_n(10), aes(x=reorder(word,tf_idf), y= tf_idf,alpha=tf_idf)) +
  geom_bar(stat="identity",fill = "grey30") +
  coord_flip() + theme_classic() + xlab("") + ylab("tf-idf") +
  scale_alpha_continuous(range = c(0.6, 1)) +
  ggtitle("",subtitle="Tim Tebow") +
  geom_text(aes(label=word), hjust=-0.1, colour="black") +
  theme(plot.subtitle=element_text(size=10, hjust=0, face="italic", color="black")) +
  theme(legend.position="none") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ylim(c(0,.25))

d<-ggplot(tweets_words  %>% filter(screenName == "williebosshog") %>% group_by(screenName) %>% top_n(10), aes(x=reorder(word,tf_idf), y= tf_idf,alpha=tf_idf)) +
  geom_bar(stat="identity",fill = "grey30") +
  coord_flip() + theme_classic() + xlab("") + ylab("tf-idf") +
  scale_alpha_continuous(range = c(0.6, 1)) +
  ggtitle("",subtitle="Willie Robertson") +
  geom_text(aes(label=word), hjust=-0.1, colour="black") +
  theme(plot.subtitle=element_text(size=10, hjust=0, face="italic", color="black")) +
  theme(legend.position="none") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ylim(c(0,.25))

e<-ggplot(tweets_words  %>% filter(screenName == "RickWarren") %>% group_by(screenName) %>% top_n(10), aes(x=reorder(word,tf_idf), y= tf_idf,alpha=tf_idf)) +
  geom_bar(stat="identity",fill = "grey30") +
  coord_flip() + theme_classic() + xlab("") + ylab("tf-idf") +
  scale_alpha_continuous(range = c(0.6, 1)) +
  ggtitle("",subtitle="Rick Warren") +
  geom_text(aes(label=word), hjust=-0.1, colour="black") +
  theme(plot.subtitle=element_text(size=10, hjust=0, face="italic", color="black")) +
  theme(legend.position="none") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ylim(c(0,.25))

f<-ggplot(tweets_words  %>% filter(screenName == "bubbawatson") %>% group_by(screenName) %>% top_n(10), aes(x=reorder(word,tf_idf), y= tf_idf,alpha=tf_idf)) +
  geom_bar(stat="identity",fill = "grey30") +
  coord_flip() + theme_classic() + xlab("") + ylab("tf-idf") +
  scale_alpha_continuous(range = c(0.6, 1)) +
  ggtitle("",subtitle="Bubba Watson") +
  geom_text(aes(label=word), hjust=-0.1, colour="black") +
  theme(plot.subtitle=element_text(size=10, hjust=0, face="italic", color="black")) +
  theme(legend.position="none") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ylim(c(0,.25))

g<-ggplot(tweets_words  %>% filter(screenName == "MaxLucado") %>% group_by(screenName) %>% top_n(10), aes(x=reorder(word,tf_idf), y= tf_idf,alpha=tf_idf)) +
  geom_bar(stat="identity",fill = "grey30") +
  coord_flip() + theme_classic() + xlab("") + ylab("tf-idf") +
  scale_alpha_continuous(range = c(0.6, 1)) +
  ggtitle("",subtitle="Max Lucado") +
  geom_text(aes(label=word), hjust=-0.1, colour="black") +
  theme(plot.subtitle=element_text(size=10, hjust=0, face="italic", color="black")) +
  theme(legend.position="none") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ylim(c(0,.25))

h<-ggplot(tweets_words  %>% filter(screenName == "lecrae") %>% group_by(screenName) %>% top_n(10), aes(x=reorder(word,tf_idf), y= tf_idf,alpha=tf_idf)) +
  geom_bar(stat="identity",fill = "grey30") +
  coord_flip() + theme_classic() + xlab("") + ylab("tf-idf") +
  scale_alpha_continuous(range = c(0.6, 1)) +
  ggtitle("",subtitle="Lecrae") +
  geom_text(aes(label=word), hjust=-0.1, colour="black") +
  theme(plot.subtitle=element_text(size=10, hjust=0, face="italic", color="black")) +
  theme(legend.position="none") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ylim(c(0,.25))

i<-ggplot(tweets_words  %>% filter(screenName == "JohnPiper") %>% group_by(screenName) %>% top_n(10), aes(x=reorder(word,tf_idf), y= tf_idf,alpha=tf_idf)) +
  geom_bar(stat="identity",fill = "grey30") +
  coord_flip() + theme_classic() + xlab("") + ylab("tf-idf") +
  scale_alpha_continuous(range = c(0.6, 1)) +
  ggtitle("",subtitle="John Piper") +
  geom_text(aes(label=word), hjust=-0.1, colour="black") +
  theme(plot.subtitle=element_text(size=10, hjust=0, face="italic", color="black")) +
  theme(legend.position="none") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ylim(c(0,.25))

j<-ggplot(tweets_words  %>% filter(screenName == "DaveRamsey") %>% group_by(screenName) %>% top_n(10), aes(x=reorder(word,tf_idf), y= tf_idf,alpha=tf_idf)) +
  geom_bar(stat="identity",fill = "grey30") +
  coord_flip() + theme_classic() + xlab("") + ylab("tf-idf") +
  scale_alpha_continuous(range = c(0.6, 1)) +
  ggtitle("",subtitle="Dave Ramsey") +
  geom_text(aes(label=word), hjust=-0.1, colour="black") +
  theme(plot.subtitle=element_text(size=10, hjust=0, face="italic", color="black")) +
  theme(legend.position="none") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ylim(c(0,.25))

#Create function for multiplot(): copied from http://bxhorn.com/ggplot-multiple-plots/
multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  # launch the grid graphical system
  require(grid)
  
  # make a list from the "..." arguments and/or plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  
  # if layout = NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # make the panel using ncol; nrow is calculated from ncol
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)), ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    # set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # put each plot, in the correct location
    for (i in 1:numPlots) {
      # get the i,j matrix position of the subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row, layout.pos.col = matchidx$col))
    }
  }
}

#print final plot:
multiplot(a,b,c,d,e,f,g,h,i,j,layout = matrix(c(1:10),nrow=2,byrow=TRUE))
