
# I remade some bar charts using slightly different code
# and added some different words of interest.
library(ggplot2)
library(plyr)

trump<- tweets[ grep("Trump", tweets$text), ]
count <- count(trump, "screenName")
ggplot(count, aes(x=reorder(screenName, freq), y = freq)) + 
  geom_bar(stat="identity") + coord_flip() + 
  ggtitle("Number of Tweets Containing 'Trump'") +
  xlab("Screen Names = 37") + ylab("Tweets = 835") +
  theme_classic()
length(unique(trump$screenName)) #37
length(trump$screenName) #835

clinton<- tweets[ grep("Clinton", tweets$text), ]
count <- count(clinton, "screenName")
ggplot(count, aes(x=reorder(screenName, freq), y = freq)) + 
  geom_bar(stat="identity") + coord_flip() + 
  ggtitle("Number of Tweets Containing 'Clinton'") +
  xlab("Screen Names = 29") + ylab("Tweets = 404") +
  theme_classic()
length(unique(clinton$screenName)) #29
length(clinton$screenName) #404

obama<- tweets[ grep("Obama", tweets$text), ]
count <- count(obama, "screenName")
ggplot(count, aes(x=reorder(screenName, freq), y = freq)) + 
  geom_bar(stat="identity") + coord_flip() + 
  ggtitle("Number of Tweets Containing 'Obama'") +
  xlab("Screen Names = 27") + ylab("Tweets = 588") +
  theme_classic()
length(unique(obama$screenName)) #27
length(obama$screenName) #588

cruz<- tweets[ grep("Cruz", tweets$text), ]
count <- count(cruz, "screenName")
ggplot(count, aes(x=reorder(screenName, freq), y = freq)) + 
  geom_bar(stat="identity") + coord_flip() + 
  ggtitle("Number of Tweets Containing 'Cruz'") +
  xlab("Screen Names = 23") + ylab("Tweets = 171") +
  theme_classic()
length(unique(cruz$screenName)) #23
length(cruz$screenName) #171

sanders<- tweets[ grep("Sanders", tweets$text), ]
count <- count(bernie, "screenName")
ggplot(count, aes(x=reorder(screenName, freq), y = freq)) + 
  geom_bar(stat="identity") + coord_flip() + 
  ggtitle("Number of Tweets Containing 'Bernie'") +
  xlab("Screen Names = 13") + ylab("Tweets = 73") +
  theme_classic()
length(unique(bernie$screenName)) #13
length(bernie$screenName) #73

politics<- tweets[ grep("politics", tweets$text), ]
count <- count(politics, "screenName")
ggplot(count, aes(x=reorder(screenName, freq), y = freq)) + 
  geom_bar(stat="identity") + coord_flip() + 
  ggtitle("Number of Tweets Containing 'Politics'") +
  xlab("Screen Names = 27") + ylab("Tweets = 65") +
  theme_classic()
length(unique(politics$screenName)) #27
length(politics$screenName) #65

govt<- tweets[ grep("government", tweets$text), ]
count <- count(dem, "screenName")
ggplot(count, aes(x=reorder(screenName, freq), y = freq)) + 
  geom_bar(stat="identity") + coord_flip() + 
  ggtitle("Number of Tweets Containing 'government'") +
  xlab("Screen Names = 25") + ylab("Tweets = 80") +
  theme_classic()
length(unique(govt$screenName)) #25
length(govt$screenName) #80

dem<- tweets[ grep("Democrat", tweets$text), ]
count <- count(dem, "screenName")
ggplot(count, aes(x=reorder(screenName, freq), y = freq)) + 
  geom_bar(stat="identity") + coord_flip() + 
  ggtitle("Number of Tweets Containing 'Democrat'") +
  xlab("Screen Names = 19") + ylab("Tweets = 82") +
  theme_classic()
length(unique(dem$screenName)) #19
length(dem$screenName) #82

rep<- tweets[ grep("Republican", tweets$text), ]
count <- count(rep, "screenName")
ggplot(count, aes(x=reorder(screenName, freq), y = freq)) + 
  geom_bar(stat="identity") + coord_flip() + 
  ggtitle("Number of Tweets Containing 'Republican'") +
  xlab("Screen Names = 19") + ylab("Tweets = 86") +
  theme_classic()
length(unique(rep$screenName)) #19
length(rep$screenName) #86

election<- tweets[ grep("election", tweets$text), ]

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
count<-count(poli1,c("screenName","Mentions"))
count1b <- count(poli1,c("screenName"))
names<-count1b[with(count1b, order(freq,decreasing=FALSE)),]
library(colorspace)
pal <- choose_palette()
ggplot(count, aes(x=reorder(screenName, freq), y = freq,fill=Mentions)) + 
  geom_bar(stat="identity") + coord_flip() + scale_fill_manual(values=pal(10)) +
  ggtitle("Number of Tweets Containing Mentions\nof Names of Politicians, Parties, Politics,\nand the Election") +
  xlab("60 Screen Names") + ylab("Tweets = 1,633") +
  theme_classic() +
  scale_x_discrete(limits=names$screenName) +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=3))
length(unique(poli1$screenName)) #60
length(poli1$screenName) #1633


immig<- tweets[ grep("immigration", tweets$text), ]
count <- count(immig, "screenName")
ggplot(count, aes(x=reorder(screenName, freq), y = freq)) + 
  geom_bar(stat="identity") + coord_flip() + 
  ggtitle("Number of Tweets Containing 'immigration'") +
  xlab("Screen Names = 11") + ylab("Tweets = 55") +
  theme_classic()
length(unique(immig$screenName)) #11
length(immig$screenName) #55

abortion<- tweets[ grep("abortion", tweets$text), ]
count <- count(abortion, "screenName")
ggplot(count, aes(x=reorder(screenName, freq), y = freq)) + 
  geom_bar(stat="identity") + coord_flip() + 
  ggtitle("Number of Tweets Containing 'abortion'") +
  xlab("Screen Names = 23") + ylab("Tweets = 227") +
  theme_classic()
length(unique(abortion$screenName)) #23
length(abortion$screenName) #227

hom<- tweets[ grep("homosexuality", tweets$text), ]
count <- count(hom, "screenName")
ggplot(count, aes(x=reorder(screenName, freq), y = freq)) + 
  geom_bar(stat="identity") + coord_flip() + 
  ggtitle("Number of Tweets Containing 'homosexuality'") +
  xlab("Screen Names = 6") + ylab("Tweets = 18") +
  theme_classic()
length(unique(hom$screenName)) #6
length(hom$screenName) #18

homo<-tweets[grep("homosexual",tweets$text),]

gay<- tweets[ grep("gay", tweets$text), ]
count <- count(gay, "screenName")
ggplot(count, aes(x=reorder(screenName, freq), y = freq)) + 
  geom_bar(stat="identity") + coord_flip() + 
  ggtitle("Number of Tweets Containing 'gay'") +
  xlab("Screen Names = 25") + ylab("Tweets = 128") +
  theme_classic()
length(unique(gay$screenName)) #25
length(gay$screenName) #128

climate<- tweets[ grep("climate change", tweets$text), ]
count <- count(climate, "screenName")
ggplot(count, aes(x=reorder(screenName, freq), y = freq)) + 
  geom_bar(stat="identity") + coord_flip() + 
  ggtitle("Number of Tweets Containing 'climate change'") +
  xlab("Screen Name = 4") + ylab("Tweets = 4") +
  theme_classic()
length(unique(climate$screenName)) #4
length(climate$screenName) #4

islam<- tweets[ grep("Islam", tweets$text), ]
count <- count(islam, "screenName")
ggplot(count, aes(x=reorder(screenName, freq), y = freq)) + 
  geom_bar(stat="identity") + coord_flip() + 
  ggtitle("Number of Tweets Containing 'Islam'") +
  xlab("Screen Names = 24") + ylab("Tweets = 172") +
  theme_classic()
length(unique(islam$screenName)) #24
length(islam$screenName) #172

isis<- tweets[ grep("ISIS", tweets$text), ]
count <- count(islam, "screenName")
ggplot(count, aes(x=reorder(screenName, freq), y = freq)) + 
  geom_bar(stat="identity") + coord_flip() + 
  ggtitle("Number of Tweets Containing 'ISIS'") +
  xlab("Screen Names = 21") + ylab("Tweets = 308") +
  theme_classic()
length(unique(isis$screenName)) #21
length(isis$screenName) #308

jihad<- tweets[ grep("jihad", tweets$text), ]
count <- count(jihad, "screenName")
ggplot(count, aes(x=reorder(screenName, freq), y = freq)) + 
  geom_bar(stat="identity") + coord_flip() + 
  ggtitle("Number of Tweets Containing 'jihad'") +
  xlab("Screen Names = 4") + ylab("Tweets = 36") +
  theme_classic()
length(unique(jihad$screenName)) #4
length(jihad$screenName) #36

terrorism<- tweets[ grep("terrorism", tweets$text), ]
count <- count(terrorism, "screenName")
ggplot(count, aes(x=reorder(screenName, freq), y = freq)) + 
  geom_bar(stat="identity") + coord_flip() + 
  ggtitle("Number of Tweets Containing 'terrorism'") +
  xlab("Screen Names = 15") + ylab("Tweets = 57") +
  theme_classic()
length(unique(terrorism$screenName)) #15
length(terrorism$screenName) #57

muslim<-tweets[grep("Muslim",tweets$text),]
tax<-tweets[grep("taxes",tweets$text),]
econ<-tweets[grep("economy",tweets$text),]
obamacare<-tweets[grep("Obamacare",tweets$text),]

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

poli1$Type<-c("General")
poli2$Type<-c("Specific Issues")

poli<-rbind(poli1,poli2)

poli35<-filter(poli,screenName == "lecrae"|screenName=="RonEdmondson"|
                 screenName=="KirkCameron"|screenName=="MattChandler74"|
                 screenName=="PastorJohnHagee"|screenName=="PastorChoco"|
                 screenName=="Sarcasticluther"|screenName=="jamesmacdonald"|
                 screenName=="bobrobertsjr"|screenName=="Chris_Hodges"|
                 screenName=="drmoore"|screenName=="pastoremase"|
                 screenName=="perrynoble"|screenName=="edstetzer"|
                 screenName=="douglaswils"|screenName=="PastorMark"|
                 screenName=="TedHaggard"|screenName=="DaveRamsey"|
                 screenName=="rdland"|screenName=="MJGerson"|
                 screenName=="DrJamesCDobson"|screenName=="RickWarren"|
                 screenName=="aigkenham"|screenName=="ScottWilliams"|
                 screenName=="JohnPiper"|screenName=="RevKevDeYoung"|
                 screenName=="sstrang"|screenName=="albertmohler"|
                 screenName=="raycomfort"|screenName=="Franklin_Graham"|
                 screenName=="tperkins"|screenName=="ralphreed"|
                 screenName=="DouthatNYT"|screenName=="DavidBartonWB"|
                 screenName=="JaySekulow")
               

count<-count(poli,c("screenName","Type","Mentions"))
count1b <- count(poli,c("screenName"))
names<-count1b[with(count1b, order(freq,decreasing=FALSE)),]
ggplot(count %>% filter(Type == "General"), aes(x=reorder(screenName, freq), y = freq,fill=Mentions)) + 
  geom_bar(stat="identity") + coord_flip() + scale_fill_grey() +
  ggtitle("Number of Tweets that Address Politics\nGenerally and Specific Political Issues") +
  xlab("64 Screen Names") + ylab("Tweets = 2,697") +
  theme_classic() +
  scale_x_discrete(limits=names$screenName) +
  theme(legend.justification=c(1,0),legend.position=c(1,0)) +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=3))
length(unique(poli$screenName)) #64
length(poli$screenName) #2697

count35<-count(poli35,c("screenName","Type","Mentions"))
count1c <- count(poli35,c("screenName"))
names35<-count1c[with(count1c, order(freq,decreasing=FALSE)),]
ggplot(count35, aes(x=reorder(screenName, freq), y = freq,fill=Type)) + 
  geom_bar(stat="identity") + coord_flip() + scale_fill_grey() +
  ggtitle("Number of Tweets that Address Politics\nGenerally and Political Issues Specifically",
          subtitle = "Top 35 Accounts among the 64 Accounts Identified") +
  xlab("") + ylab("Total Tweets = 2,629") +
  theme_classic() +
  scale_x_discrete(limits=names35$screenName) +
  theme(legend.justification=c(1,0),legend.position=c(1,0)) +
  guides(fill=guide_legend(title=NULL)) +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=3))
length(poli35$screenName) #2629

count1d <- count(poli,c("Mentions"))
namesd<-count1d[with(count1d, order(freq,decreasing=FALSE)),]
ggplot(count35, aes(x=reorder(Mentions, freq), y = freq,fill=Type)) + 
  geom_bar(stat="identity") + coord_flip() + scale_fill_grey() +
  ggtitle("Number of Political Tweets",
          subtitle = "Selection of Terms Associated with Political Figures\nand Issues") +
  xlab("") + ylab("Total Tweets = 2,629") +
  theme_classic() +
  scale_x_discrete(limits=namesd$Mentions) +
  theme(legend.justification=c(1,0),legend.position=c(1,0)) +
  guides(fill=guide_legend(title=NULL)) +
  theme(panel.grid.major.x=element_line(colour="grey50",linetype=3))
length(poli35$screenName) #2629


names35<-c("lecrae","RonEdmondson","KirkCameron","MattChandler74","PastorJohnHagee",
           "PastorChoco","Sarcasticluther","jamesmacdonald","bobrobertsjr","Chris_Hodges",
           "drmoore","pastoremase","perrynoble","edstetzer","douglaswils","PastorMark",
           "TedHaggard","DaveRamsey","rdland","MJGerson","DrJamesCDobson","RickWarren",
           "aigkenham","ScottWilliams","JohnPiper","RevKevDeYoung","sstrang","albertmohler",
           "raycomfort","Franklin_Graham","tperkins","ralphreed","DouthatNYT","DavidBartonWB",
           "JaySekulow")
