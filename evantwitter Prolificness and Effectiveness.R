
## Some Analysis of Evangelical Tweets.

# After loading the repository, load the data.
tweets<-read.csv("total.csv")

# Filter out politicians.
library(dplyr)
tweets <- filter(tweets, screenName != "realDonaldTrump")
tweets <- filter(tweets, screenName != "tedcruz")
tweets <- filter(tweets, screenName != "GovMikeHuckabee")
tweets <- filter(tweets, screenName != "GaryLBauer")

# Make sure the dates and times the tweets were created are in the 
# right format.
tweets$created<-as.POSIXlt(tweets$created)
tweets$created<-as.POSIXct(tweets$created)

# Make identifiers for whether tweets have been retweeted and favorited.
library(car)
tweets$retweeted<-Recode(tweets$retweetCount,"0='FALSE';else='TRUE'")
tweets$favorited<-Recode(tweets$favoriteCount,"0='FALSE';else='TRUE'")

# Upload dataset with the number of followers per evangelical, and merge the data.
clean_sheet<-read.csv("clean_sheet_new.csv",stringsAsFactors = FALSE)
tweets <- merge(tweets, clean_sheet, by=c("screenName"))

# Finally, create a dataset of aggregated data per twitter account.
aggdata <-aggregate(merged, by=list(merged$screenName), 
                    FUN=mean, na.rm=TRUE)
aggdata35 <- filter(aggdata, followers >= 140000) # Top 35 accounts by 
                                                  # number of followers.

# Increase in Twitter Usage by Evangelicals over Time
ggplot(tweets, aes(created)) + geom_histogram(bins=405) + 
  ggtitle("Increase in Twitter Usage by Evangelical Leaders over Time",
          subtitle="Start Date:  December 13, 2008\nEnd Date:  September 17, 2016") +
  xlab("Date Tweeted") + ylab("Total Number of Tweets") +
  theme_classic() + theme(panel.grid.major.y=element_line(colour="grey50",linetype=2))

# Prolificness and Effectiveness
tweets2<-filter(tweets,followers >= 140000)
tweets3<-filter(tweets2,replyCat != "NA")
count1 <- count(tweets %>% filter(followers >= 140000),c("screenName"))
count1b <- count(tweets3,c("screenName"))
count2 <- count(tweets %>% filter(followers >= 140000),c("screenName",
                                                         "retweeted",
                                                         "favorited",
                                                         "replyCat"))
count3 <- count(tweets3,
                c("screenName","replyCat"))
                                                         
names<-count1[with(count1, order(freq,decreasing=TRUE)),]
names2<-count1b[with(count1b, order(freq,decreasing=TRUE)),]
names3<-count3[with(count3, order(freq,decreasing=TRUE)),]

    # Retweets
ggplot(count2, aes(x=screenName,y=freq,fill=retweeted)) + 
  geom_bar(stat="identity") + 
  ggtitle("Top 35 Evangelical Leaders Ordered from Most to Least Prolific",
          subtitle="Whose Tweets Are Most Retweeted?") +
  xlab("") + ylab("Count") +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=c("grey75","grey25"),
                    labels=c("Not Retweeted","Retweeted")) + theme_classic() +
  scale_x_discrete(limits=names$screenName) +
  theme(axis.text.x=element_text(angle=90)) +
  theme(legend.justification=c(.9,.825),legend.position=c(.9,.825)) +
  theme(panel.grid.major.y=element_line(colour="grey50",linetype=2))

    # Favorites
ggplot(count2, aes(x=screenName,y=freq,fill=favorited)) + 
  geom_bar(stat="identity") + 
  ggtitle("Top 35 Evangelical Leaders Ordered from Most to Least Prolific",
          subtitle="Whose Tweets Are Most Likely to Be Favorited?") +
  xlab("") + ylab("Count") +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(values=c("grey75","grey25"),
                    labels=c("Not Favorited","Favorited")) + theme_classic() +
  scale_x_discrete(limits=names$screenName) +
  theme(axis.text.x=element_text(angle=90)) +
  theme(legend.justification=c(.9,.825),legend.position=c(.9,.825)) +
  theme(panel.grid.major.y=element_line(colour="grey50",linetype=2))

# Replies
ggplot(count3, aes(x=screenName,y=freq,fill=replyCat)) + 
  geom_bar(stat="identity") + 
  ggtitle("Top 35 Evangelical Leaders Ordered from Most to Least Prolific",
          subtitle="Who Responds to Tweets?") +
  xlab("") + ylab("Count") +
  guides(fill=guide_legend(title=NULL)) +theme_classic() +
  scale_x_discrete(limits=names2$screenName) +
  theme(axis.text.x=element_text(angle=90)) +
  scale_fill_grey() +
  theme(legend.justification=c(.9,.825),legend.position=c(.9,.825)) +
  theme(panel.grid.major.y=element_line(colour="grey50",linetype=2))


# Who is most "effective?"
    # All Twitter Accounts:
ggplot(aggdata,aes(x=retweetCount,y=favoriteCount)) +
  ggtitle("Effectiveness of Evangelical Twitter Accounts",
          subtitle="Effectiveness Measured by the Mean Number of Retweets per Tweet\nand the Mean Number of Favorites Received per Tweet.\n
          Larger Point Size = More Followers") +
  xlab("Mean Number of Retweets (on a log10 scale)") +
  ylab("Mean Number of Favoriteds (on a log10 scale)") +
  geom_point(color="red",size=0.0000025*aggdata$followers,alpha=I(.55)) + geom_text_repel(aes(label=Group.1),size=3,
                                                                       box.padding = unit(0.5, 'lines'),
                                                                       point.padding = unit(1.6, 'lines'),
                                                                       segment.color = "grey35",
                                                                       segment.size = 0.5,
                                                                       arrow = arrow(length = unit(0.01, 'npc')),
                                                                       force = 1,
                                                                       max.iter = 3e3) +
  scale_x_log10() + scale_y_log10() + theme_classic() + 
  theme(panel.grid.major=element_line(colour="grey50",linetype=2))

    # Top 35 Twitter Accounts:
ggplot(aggdata35,aes(x=retweetCount,y=favoriteCount)) +
  ggtitle("Effectiveness of Top 35 Evangelical Twitter Accounts",
          subtitle="Effectiveness Measured by the Mean Number of Retweets per Tweet\nand the Mean Number of Favoriteds Received per Tweet.\n
          Larger Point Size = More Followers") +
  xlab("Mean Number of Retweets (on a log10 scale)") +
  ylab("Mean Number of Favoriteds (on a log10 scale)") +
  geom_point(color="grey15",size=0.0000025*aggdata35$followers,alpha=I(.55)) + geom_text_repel(aes(label=Group.1),size=3.5,
                                                                          box.padding = unit(0.5, 'lines'),
                                                                          point.padding = unit(1.6, 'lines'),
                                                                          segment.color = "grey35",
                                                                          segment.size = 0.5,
                                                                          arrow = arrow(length = unit(0.01, 'npc')),
                                                                          force = 1,
                                                                          max.iter = 3e3) +
  scale_x_log10() + scale_y_log10() + theme_classic() + 
  theme(panel.grid.major=element_line(colour="grey50",linetype=2))

count4 <- count(tweets3,
                c("screenName","replyToSID"))

# Top 35 Twitter Accounts:
ggplot(aggdata %>% filter(followers >= 140000),aes(y=totaltweets,x=100*(replyToSID/totaltweets))) +
  geom_point() + geom_smooth() + theme_classic() + 
  scale_x_continuous(labels=scales::comma_format()) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  theme(panel.grid.major=element_line(colour="grey75",linetype=3)) +
  scale_x_log10() +
  ylab("Total Number of Tweets") +
  xlab("Percentage of the Average Number of Replies per Tweet\nper Total Number of Tweets on a log10 Scale") +
  ggtitle("Share of Replies per Tweet by Total Number of Tweets",
          subtitle = "Those who tweet less have a more efficient response rate.\n
          Note:  There are numerous missing values for replies.") +
  geom_text_repel(aes(label=Group.1),size=3,
                  box.padding = unit(.75, 'lines'),
                  point.padding = unit(1, 'lines'),
                  segment.color = "#88888870",
                  segment.size = 0.5,
                  arrow = arrow(length = unit(0.01, 'npc')),
                  force = 1,
                  max.iter = 3e3)

rt <- aggdata
fav <- aggdata
rt$score <- rt$rtsum/rt$totaltweets
fav$score <- fav$favsum/fav$totaltweets
rt$label <- c("Retweets")
fav$label <- c("Favorites")
dfagg <- rbind(fav, rt)

# Top 35 Twitter Accounts:
ggplot(dfagg,aes(x=replyToSID,y=score,fill=label)) +
  geom_point() +  theme_classic() +
  theme(axis.text=element_text(angle=45)) +
  theme(panel.grid.major=element_line(colour="grey50",linetype=2)) +
  xlab("Mean Number of Replies per Tweet") +
  ylab("Favorite Score") +
  ggtitle("Relationship between Response Rate and Favorite Score",
          subtitle = "Note:  There are numerous missing values for replies.")

# Top 35 Twitter Accounts:
ggplot(dfagg,aes(x=replyToSID,y=rtscore,fill=label)) +
  geom_point() + theme_classic() +
  theme(axis.text=element_text(angle=45)) +
  theme(panel.grid.major=element_line(colour="grey50",linetype=2)) +
  xlab("Mean Number of Replies per Tweet") +
  ylab("Retweet Score") +
  ggtitle("Relationship between Response Rate and Retweet Score",
          subtitle = "Note:  There are numerous missing values for replies.")

#Putting Retweet Score and Favorite Score in One Plot
rt <- clean_sheet
fav <- clean_sheet

rt$score <- rt$rtsum/rt$totaltweets
fav$score <- fav$favsum/fav$totaltweets

rt$label <- c("Retweets")
fav$label <- c("Favorites")
df <- rbind(fav, rt)
ggplot(df %>% filter(followers >= 140000), aes(x=reorder(screenName, score), y = score)) + 
  geom_bar(aes(fill=label),stat="identity", position= "dodge") + 
  scale_fill_manual(values=c("grey75","grey25")) +
  xlab("") + ylab("Retweet and Favorited Score") +
  guides(fill=guide_legend(title=NULL)) +
  ggtitle("Retweet and Favorited Scores for Top 35 Evangelical Leaders",
          subtitle="Scores Based on the Sum of Retweet Counts and Favorited Counts Divided\nby the Total Number of Tweets per Twitter Account Respectively.") +
  theme_classic() + 
  theme(legend.justification=c(.1,.8),legend.position=c(.1,.8)) +
  theme(axis.text.x=element_text(angle=90)) +
  theme(panel.grid.major.y=element_line(colour="grey50",
                                        linetype=2)) 

## Who Responds to Tweets?
tweets$replyCat <- NA
tweets$replyCat[tweets$replyToSID >= 1.395e+09] <- "1st Quartile"
tweets$replyCat[tweets$replyToSID > 5.552e+17] <- "2nd Quartile"
tweets$replyCat[tweets$replyToSID > 6.276e+17] <- "3rd Quartile"
tweets$replyCat[tweets$replyToSID > 7.452e+17] <- "4th Quartile"
aggdata$replyDum <- NA
aggdata$replyDum[aggdata$replyToSID >= 1.395e+09] <- "1st Quartile"
aggdata$replyDum[aggdata$replyToSID > 5.552e+17] <- "2nd Quartile"
aggdata$replyDum[aggdata$replyToSID > 6.276e+17] <- "3rd Quartile"
aggdata$replyDum[aggdata$replyToSID > 7.452e+17] <- "4th Quartile"

aggdata$followersDum <- NA
aggdata$followersDum[aggdata$followers >= 938] <- "1st Quartile"
aggdata$followersDum[aggdata$followers > 39100] <- "2nd Quartile"
aggdata$followersDum[aggdata$followers < 543500] <- "Less than Mean"
aggdata$followersDum[aggdata$followers > 525000] <- "3rd Quartile"

aggdata.prop<-data.frame(prop.table(table(aggdata$replyDum,
                            aggdata$followers),1))
ggplot(aggdata.prop,aes(Var1,Freq,fill=Var2)) +
  geom_bar(position="stack",stat="identity") + theme_classic() 
  

ggplot(aggdata %>% filter(replyDum != "NA"),aes(x=replyDum,y=followers)) +
  ggtitle("Responses by Followers") +
  xlab("Responses") +
  ylab("Followers") +
  geom_boxplot(varwidth = TRUE) + geom_jitter() + theme_classic() 

ggplot(aggdata %>% filter(followersDum != "NA"),aes(x=followersDum,y=replyToSID)) +
  ggtitle("Responses by Followers") +
  xlab("Followers") +
  ylab("Responses") +
  geom_boxplot(varwidth = TRUE) + geom_jitter() + theme_classic() 

qplot(replyDum,followers,data=aggdata,geom=c("boxplot","jitter"),alpha=I(.1))

