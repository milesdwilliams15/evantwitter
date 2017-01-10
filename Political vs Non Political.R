
## Political vs. Non-Political Tweets
library(ggplot2)
library(plyr)
# Create data frame of political tweets
# General
trump<- tweets[ grep("Trump", tweets$text,ignore.case=TRUE), ]
clinton<- tweets[ grep("Clinton", tweets$text,ignore.case=TRUE), ]
obama<- tweets[ grep("Obama", tweets$text,ignore.case=TRUE), ]
cruz<- tweets[ grep("Cruz", tweets$text,ignore.case=TRUE), ]
sanders<- tweets[ grep("Sanders", tweets$text,ignore.case=TRUE), ]
politics<- tweets[ grep("politics", tweets$text,ignore.case=TRUE), ]
govt<- tweets[ grep("government", tweets$text,ignore.case=TRUE), ]
dem<- tweets[ grep("Democrat", tweets$text,ignore.case=TRUE), ]
rep<- tweets[ grep("Republican", tweets$text,ignore.case=TRUE), ]
election<- tweets[ grep("election", tweets$text,ignore.case=TRUE), ]

# Issues
immig<- tweets[ grep("immigration", tweets$text,ignore.case=TRUE), ]
tax<- tweets[ grep("taxes",tweets$text,ignore.case=TRUE), ]
econ<- tweets[ grep("economy",tweets$text,ignore.case=TRUE), ]
obamacare<-tweets[ grep("Obamacare",tweets$text,ignore.case=TRUE),]
abortion<- tweets[ grep("abortion", tweets$text,ignore.case=TRUE), ]
hom<- tweets[ grep("homosexuality", tweets$text,ignore.case=TRUE), ]
homo<-tweets[grep("homosexual",tweets$text,ignore.case=TRUE),]
gay<- tweets[ grep("gay", tweets$text,ignore.case=TRUE), ]
climate<- tweets[ grep("climate change",tweets$text,ignore.case=TRUE), ]
muslim<-tweets[grep("Muslim",tweets$text,ignore.case=TRUE),]
islam<- tweets[ grep("Islam", tweets$text,ignore.case=TRUE), ]
isis<- tweets[ grep("ISIS", tweets$text,ignore.case=TRUE), ]
jihad<- tweets[ grep("jihad", tweets$text,ignore.case=TRUE), ]
terrorism<- tweets[ grep("terrorism", tweets$text,ignore.case=TRUE), ]


# Data frame of general politics
trump$Mentions<-c("Trump")
obama$Mentions<-c("Obama")
clinton$Mentions<-c("Clinton")
cruz$Mentions<-c("Cruz")
sanders$Mentions<-c("Sanders")
politics$Mentions<-c("politics")
govt$Mentions<-c("government")
dem$Mentions<-c("Democrat")
rep$Mentions<-c("Republican")
election$Mentions<-c("election")
poli1<-rbind(trump,obama,clinton,cruz,sanders,politics,govt,dem,rep,election)

# Data frame of political issues
muslim$Mentions<-c("Muslim")
tax$Mentions<-c("taxes")
econ$Mentions<-c("economy")
obamacare$Mentions<-c("Obamacare")
terrorism$Mentions<-c("terrorism")
jihad$Mentions<-c("jihad")
isis$Mentions<-c("ISIS")
islam$Mentions<-c("Islam")
climate$Mentions<-c("climate change")
gay$Mentions<-c("gay")
homo$Mentions<-c("homosexual")
hom$Mentions<-c("homosexuality")
abortion$Mentions<-c("abortion")
immig$Mentions<-c("immigration")
poli2<-rbind(muslim,tax,econ,obamacare,terrorism,jihad,isis,islam,climate,
             gay,homo,hom,abortion,immig)

# Add a vector of labels
poli1$Type<-c("General Politics")
poli2$Type<-c("Specific Issues")

# Combine the data frames
poli<-rbind(poli1,poli2)

# Create a data frame that doesn't contain the above political terms
# General
ntrump<- tweets[ grep("Other", tweets$text,ignore.case=TRUE,invert=TRUE), ]
nclinton<- tweets[ grep("Other", tweets$text,ignore.case=TRUE,invert=TRUE), ]
nobama<- tweets[ grep("Other", tweets$text,ignore.case=TRUE,invert=TRUE), ]
ncruz<- tweets[ grep("Other", tweets$text,ignore.case=TRUE,invert=TRUE), ]
nsanders<- tweets[ grep("Other", tweets$text,ignore.case=TRUE,invert=TRUE), ]
npolitics<- tweets[ grep("Other", tweets$text,ignore.case=TRUE,invert=TRUE), ]
ngovt<- tweets[ grep("Other", tweets$text,ignore.case=TRUE,invert=TRUE), ]
ndem<- tweets[ grep("Other", tweets$text,ignore.case=TRUE,invert=TRUE), ]
nrep<- tweets[ grep("Other", tweets$text,ignore.case=TRUE,invert=TRUE), ]
nelection<- tweets[ grep("Other", tweets$text,ignore.case=TRUE,invert=TRUE), ]

# Issues
nimmig<- tweets[ grep("immigration", tweets$text,ignore.case=TRUE,invert=TRUE), ]
ntax<- tweets[ grep("taxes",tweets$text,ignore.case=TRUE), ]
necon<- tweets[ grep("economy",tweets$text,ignore.case=TRUE), ]
nobamacare<-tweets[ grep("Obamacare",tweets$text,ignore.case=TRUE),]
nabortion<- tweets[ grep("abortion", tweets$text,ignore.case=TRUE,invert=TRUE), ]
nhom<- tweets[ grep("homosexuality", tweets$text,ignore.case=TRUE,invert=TRUE), ]
nhomo<-tweets[grep("homosexual",tweets$text,ignore.case=TRUE,invert=TRUE),]
ngay<- tweets[ grep("gay", tweets$text,ignore.case=TRUE,invert=TRUE), ]
nclimate<- tweets[ grep("climate change", tweets$text,ignore.case=TRUE,invert=TRUE), ]
nmuslim<-tweets[grep("Muslim",tweets$text,ignore.case=TRUE,invert=TRUE),]
nislam<- tweets[ grep("Islam", tweets$text,ignore.case=TRUE,invert=TRUE), ]
nisis<- tweets[ grep("ISIS", tweets$text,ignore.case=TRUE,invert=TRUE), ]
njihad<- tweets[ grep("jihad", tweets$text,ignore.case=TRUE,invert=TRUE), ]
nterrorism<- tweets[ grep("terrorism", tweets$text,ignore.case=TRUE,invert=TRUE), ]
npoli<-rbind(ntrump,nobama,nclinton,ncruz,nsanders,npolitics,ngovt,ndem,nrep,nelection,
             nmuslim,ntax,necon,nobamacare,nterrorism,njihad,nisis,nislam,nclimate,
             ngay,nhomo,nhom,nabortion,nimmig)

# Add labels
npoli$Mentions<-c("Other")
npoli$Type<-c("Other")
npoli$Political<-c("Non-Political")
poli$Political<-c("Political")
# Combine political with non-political data:
poliCombo<-rbind(poli,npoli)


## Political vs. Non-Political Accounts:
length(unique(tweets$screenName)) # 88 Total screen names
length(unique(poli$screenName))   # 70 Total screen names
count<-rbind(18,70)
Political<-rbind("18 Non-Political","70 Political")
df<-data.frame(cbind(count,Political))
ggplot(df, aes(x=reorder(Political, count), y = count,fill=Political)) + 
  geom_bar(stat="identity") + coord_flip() + scale_fill_grey() +
  ggtitle("Number of Political vs. Non-Political Twitter Accounts",
          subtitle="Based on Mentions of Selected Political Terms") +
  xlab("") + ylab("") +
  geom_text(aes(label=Political), hjust=1.1, colour=c("grey75","grey25"),
            size=5) +
  theme_classic() +
  theme(legend.position="none") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=3)) 

## Counts for Total Political Tweets vs. Other: type of political tweet divided
count<-count(poliCombo,"Type")
count$freq # to get the values
count$freq2<-c("1,824","1,768,362","1,493") # add some commas
ggplot(count, aes(x=reorder(Type,freq),y=freq)) + 
  geom_bar(stat="identity") + coord_flip() + scale_fill_grey() +
  ggtitle("Number of Political vs. non-Political Tweets",
          subtitle="Based on Mentions of Selected Political Terms") +
  xlab("") + ylab("Total Tweets = 1,771,679") +
  theme_classic() + 
  theme(legend.justification=c(1,0),legend.position=c(1,0)) +
  geom_text(aes(label=freq2), hjust=c(0,1.1,0), colour=c("black","grey75","black")) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=3)) +
  scale_y_continuous(labels=scales::comma_format()) 
length(unique(poliCombo$screenName)) #88
length(poliCombo$screenName) #1,771,679

## Counts for Total Political Tweets vs. Other: type of political tweet divided
count<-count(poliCombo,"Political")
count$freq # to get the values
count$freq2<-c("1,768,362","3,317") # add some commas
ggplot(count, aes(x=reorder(Political,freq),y=freq)) + 
  geom_bar(stat="identity") + coord_flip() + scale_fill_grey() +
  ggtitle("Number of Political vs. non-Political Tweets",
          subtitle="Based on Mentions of Selected Political Terms") +
  xlab("") + ylab("Total Tweets = 1,771,679") +
  theme_classic() + 
  theme(legend.justification=c(1,0),legend.position=c(1,0)) +
  geom_text(aes(label=freq2), hjust=c(1.1,0), colour=c("grey75","black")) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=3)) +
  scale_y_continuous(labels=scales::comma_format()) 


## Counts for General Politics vs. Specific Issues:
library(dplyr)
poli35<- poli %>% filter(screenName=="aigkenham"|screenName=="albertmohler"|screenName=="bobrobertsjr"|screenName=="Chris_Hodges"|screenName=="chuckswindoll"|screenName=="dancathy"|screenName=="DaveRamsey"|screenName=="DavidBartonWB"|screenName=="douglaswils"|screenName=="DouthatNYT"|screenName=="DrJamesCDobson"|screenName=="drmoore"|screenName=="edstetzer"|screenName=="Franklin_Graham"|screenName=="jamesmacdonald"|screenName=="JaySekulow"|screenName=="johnmacarthur"|screenName=="JohnPipe"|screenName=="KirkCameron"|screenName=="MJGerson"|screenName=="PastorChoco"|screenName=="pastoremase"|screenName=="PastorMark"|screenName=="perrynoble"|screenName=="ralphreed"|screenName=="raycomfort"|screenName=="rdland"|screenName=="RevKevDeYoung"|screenName=="RickWarren"|screenName=="Sarcasticluther"|screenName=="ScottWilliams"|screenName=="sstrang"|screenName=="TedHaggard"|screenName=="tperkins")
detach("package:dplyr")
count<-count(poli35,c("screenName","Type"))
count1b <- count(poli35,c("screenName"))
names<-count1b[with(count1b, order(freq,decreasing=FALSE)),]
ggplot(count, aes(x=reorder(screenName, freq), y = freq,fill=Type)) + 
  geom_bar(stat="identity") + coord_flip() + scale_fill_grey() +
  ggtitle("Number of Tweets that Address General Politics\nvs. Specific Issues",
          subtitle = "Top 35 out of a Total of 70 Evangelicals that Talk about Politics") +
  xlab("") + ylab("3,125 Tweets out of a Total of 3,317") +
  theme_classic() + guides(fill=guide_legend(title=NULL)) +
  scale_x_discrete(limits=names$screenName) +
  theme(legend.justification=c(1,0),legend.position=c(1,0)) +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=3))
length(unique(poli$screenName)) #70
length(poli$screenName) #3,317
length(poli35$screenName) #3,125

## General Politics Mentions
count<-count(poli,c("Mentions","Type"))
count1b<-count(poli,c("Mentions"))
names<-count1b[with(count1b, order(freq,decreasing=FALSE)),]
ggplot(count, aes(x=reorder(Mentions, freq), y = freq,fill=Type)) + 
  geom_bar(stat="identity") + coord_flip() + scale_fill_grey() +
  ggtitle("Number of Tweets that Address General Politics\nand Specific Issues",
          subtitle="By Term") +
  xlab("") + ylab("3,317 Total Tweets") +
  theme_classic() + guides(fill=guide_legend(title=NULL)) +
  scale_x_discrete(limits=names$Mentions) +
  theme(legend.justification=c(1,0),legend.position=c(1,0)) +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=3)) +
  scale_y_continuous(labels=scales::comma_format()) 

## General Issues Mentions
library(dplyr)
poli35.1<- poli1 %>% filter(screenName=="aigkenham"|screenName=="albertmohler"|screenName=="bobrobertsjr"|screenName=="Chris_Hodges"|screenName=="chuckswindoll"|screenName=="dancathy"|screenName=="DaveRamsey"|screenName=="DavidBartonWB"|screenName=="douglaswils"|screenName=="DouthatNYT"|screenName=="DrJamesCDobson"|screenName=="drmoore"|screenName=="edstetzer"|screenName=="Franklin_Graham"|screenName=="jamesmacdonald"|screenName=="JaySekulow"|screenName=="johnmacarthur"|screenName=="JohnPipe"|screenName=="KirkCameron"|screenName=="MJGerson"|screenName=="PastorChoco"|screenName=="pastoremase"|screenName=="PastorMark"|screenName=="perrynoble"|screenName=="ralphreed"|screenName=="raycomfort"|screenName=="rdland"|screenName=="RevKevDeYoung"|screenName=="RickWarren"|screenName=="Sarcasticluther"|screenName=="ScottWilliams"|screenName=="sstrang"|screenName=="TedHaggard"|screenName=="tperkins")
detach("package:dplyr")
count<-count(poli35.1,c("screenName","Mentions"))
count1b <- count(poli35.1,c("screenName"))
names<-count1b[with(count1b, order(freq,decreasing=FALSE)),]
ggplot(count, aes(x=reorder(screenName, freq), y = freq,fill=Mentions)) + 
  geom_bar(stat="identity") + coord_flip() + scale_fill_grey() +
  ggtitle("Number of Tweets that Address General Politics",
          subtitle="Top 35 out of a Total of 70 Evangelicals that Talk about Politics") +
  xlab("") + ylab("1,744 Tweets out of a Total of 1,824") +
  theme_classic() +
  scale_x_discrete(limits=names$screenName) +
  theme(legend.justification=c(1,0),legend.position=c(1,0)) +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=3))
length(unique(poli1$screenName)) #64
length(poli35.1$screenName) #1,744 out of 1,824

## Specific Issues Mentions
library(dplyr)
poli35.2<- poli2 %>% filter(screenName=="aigkenham"|screenName=="albertmohler"|screenName=="bobrobertsjr"|screenName=="Chris_Hodges"|screenName=="chuckswindoll"|screenName=="dancathy"|screenName=="DaveRamsey"|screenName=="DavidBartonWB"|screenName=="douglaswils"|screenName=="DouthatNYT"|screenName=="DrJamesCDobson"|screenName=="drmoore"|screenName=="edstetzer"|screenName=="Franklin_Graham"|screenName=="jamesmacdonald"|screenName=="JaySekulow"|screenName=="johnmacarthur"|screenName=="JohnPipe"|screenName=="KirkCameron"|screenName=="MJGerson"|screenName=="PastorChoco"|screenName=="pastoremase"|screenName=="PastorMark"|screenName=="perrynoble"|screenName=="ralphreed"|screenName=="raycomfort"|screenName=="rdland"|screenName=="RevKevDeYoung"|screenName=="RickWarren"|screenName=="Sarcasticluther"|screenName=="ScottWilliams"|screenName=="sstrang"|screenName=="TedHaggard"|screenName=="tperkins")
detach("package:dplyr")
count<-count(poli35.2,c("screenName","Mentions"))
count1b <- count(poli35.2,c("screenName"))
names<-count1b[with(count1b, order(freq,decreasing=FALSE)),]
ggplot(count, aes(x=reorder(screenName, freq), y = freq,fill=Mentions)) + 
  geom_bar(stat="identity") + coord_flip() + scale_fill_grey() +
  ggtitle("Number of Tweets that Address General Politics",
          subtitle="Top 35 out of a Total of 70 Evangelicals that Talk about Politics") +
  xlab("") + ylab("1,381 Tweets out of a Total of 1,493") +
  theme_classic() +
  scale_x_discrete(limits=names$screenName) +
  theme(legend.justification=c(1,0),legend.position=c(1,0)) +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=3))
length(unique(poli2$screenName)) #57
length(poli35.2$screenName) #1,381 out of 1,493
