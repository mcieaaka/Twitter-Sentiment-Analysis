library(ggplot2)
library(readr)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(biclust)
library(cluster)
library(igraph)
library(fpc)

library(gridExtra)
library(cowplot)
library(reshape2)
library(scales)
library(NLP)
library(readxl)
Dataset <- read_excel("D:/sem4/A-SIN/PROJECT/Dataset.xlsx")


airline<-Dataset
str(airline)

#Histogram plot showing the number of tweets and sentiment each airline

airlineSentiment = as.data.frame(table(airline$airline,airline$airline_sentiment))
colnames(airlineSentiment) = c("Airline","Sentiment","Freq")

colours = c("firebrick1","deepskyblue","chartreuse3")

histPlot2 = ggplot(airlineSentiment) + aes(x=Airline,y=Freq,fill=Sentiment) + scale_fill_manual(values=c("indianred1","deepskyblue","chartreuse3"))
histPlot2 = histPlot2 + geom_bar(stat="identity") 
histPlot2

# 2.1. Individual pie charts for each Airline
sentAmerican = subset(airlineSentiment, Airline == "American")
American = ggplot(sentAmerican) + aes(x="American", y=Freq, fill=Sentiment) +
  geom_bar(stat="identity") +
  coord_polar("y") +
  theme(axis.text.x=element_text(color="black")) +
  theme(axis.ticks=element_blank(), axis.title=element_blank(), axis.text.y=element_blank())

y.breaks = cumsum(sentAmerican$Freq) - sentAmerican$Freq/2

American = American + scale_y_continuous(breaks=y.breaks, labels=sentAmerican$Sentiment) +
  ggtitle("American") + theme(plot.title = element_text(face="bold")) + scale_fill_manual(values=c("indianred1","deepskyblue","chartreuse3"))

# DELTA AIRLINES 
sentDelta = subset(airlineSentiment, Airline == "Delta")
Delta = ggplot(sentDelta) + aes(x="Delta", y=Freq, fill=Sentiment) +
  geom_bar(stat="identity") +
  coord_polar("y") +
  theme(axis.text.x=element_text(color="black")) +
  theme(axis.ticks=element_blank(), axis.title=element_blank(), axis.text.y=element_blank())

y.breaks = cumsum(sentDelta$Freq) - sentDelta$Freq/2

Delta = Delta + scale_y_continuous(breaks=y.breaks, labels=sentDelta$Sentiment) +
  ggtitle("Delta") + theme(plot.title = element_text(face="bold")) + scale_fill_manual(values=c("indianred1","deepskyblue","chartreuse3"))

# SOUTHWEST AIRLINES  
sentSouthwest = subset(airlineSentiment, Airline == "Southwest")
Southwest = ggplot(sentSouthwest) + aes(x="Southwest", y=Freq, fill=Sentiment) +
  geom_bar(stat="identity") +
  coord_polar("y") +
  theme(axis.text.x=element_text(color="black")) +
  theme(axis.ticks=element_blank(), axis.title=element_blank(), axis.text.y=element_blank())

y.breaks = cumsum(sentSouthwest$Freq) - sentSouthwest$Freq/2

Southwest = Southwest + scale_y_continuous(breaks=y.breaks, labels=sentSouthwest$Sentiment) +
  ggtitle("Southwest") + theme(plot.title = element_text(face="bold")) + scale_fill_manual(values=c("indianred1","deepskyblue","chartreuse3"))

# UNITED AIRLINES  
sentUnited = subset(airlineSentiment, Airline == "United")
United = ggplot(sentUnited) + aes(x="United", y=Freq, fill=Sentiment) +
  geom_bar(stat="identity") +
  coord_polar("y") +
  theme(axis.text.x=element_text(color="black")) +
  theme(axis.ticks=element_blank(), axis.title=element_blank(), axis.text.y=element_blank())

y.breaks = cumsum(sentUnited$Freq) - sentUnited$Freq/2

United = United + scale_y_continuous(breaks=y.breaks, labels=sentUnited$Sentiment) +
  ggtitle("United") + theme(plot.title = element_text(face="bold")) + scale_fill_manual(values=c("indianred1","deepskyblue","chartreuse3"))

# VIRGIN AMERICA 
sentVAirways = subset(airlineSentiment, Airline == "Virgin America")
VAirways = ggplot(sentVAirways) + aes(x="Virgin America", y=Freq, fill=Sentiment) +
  geom_bar(stat="identity") +
  coord_polar("y") +
  theme(axis.text.x=element_text(color="black")) +
  theme(axis.ticks=element_blank(), axis.title=element_blank(), axis.text.y=element_blank())

y.breaks = cumsum(sentVAirways$Freq) - sentVAirways$Freq/2

VAirways = VAirways + scale_y_continuous(breaks=y.breaks, labels=sentVAirways$Sentiment) +
  ggtitle("Virgin America") + theme(plot.title = element_text(face="bold")) + scale_fill_manual(values=c("indianred1","deepskyblue","chartreuse3"))

# US AIRWAYS  
sentUSAirways = subset(airlineSentiment, Airline == "US Airways")
USAirways = ggplot(sentUSAirways) + aes(x="US Airways", y=Freq, fill=Sentiment) +
  geom_bar(stat="identity") +
  coord_polar("y") +
  theme(axis.text.x=element_text(color="black")) +
  theme(axis.ticks=element_blank(), axis.title=element_blank(), axis.text.y=element_blank())

y.breaks = cumsum(sentUSAirways$Freq) - sentUSAirways$Freq/2

USAirways = USAirways + scale_y_continuous(breaks=y.breaks, labels=sentUSAirways$Sentiment) +
  ggtitle("US Airways") + theme(plot.title = element_text(face="bold")) + scale_fill_manual(values=c("indianred1","deepskyblue","chartreuse3"))


plot_grid(American,Delta,Southwest,United,VAirways,USAirways,ncol=2,nrow=3) 




# 3. Initial Global analysis

table(airline$negativereason, airline$airline)

globalSentReasons = as.data.frame(table(airline$negativereason, airline$airline))
colnames(globalSentReasons) = c("Reason","Airline", "Freq")

ggplot(globalSentReasons) + aes(y = Freq, x = Reason, group = Airline, colour = Airline) + coord_polar() + geom_point() + geom_path() + labs(x = NULL)


#CALCULATE PERCENTAGES
aggregate(Freq ~ Airline, globalSentReasons, sum)
globalSentReasons$TotalTwAirline = 0
globalSentReasons[1:11,4] = 2759
globalSentReasons[12:22,4] = 2222
globalSentReasons[23:33,4] = 2420
globalSentReasons[34:44,4] = 3822
globalSentReasons[45:55,4] = 2913
globalSentReasons[56:66,4] = 503
globalSentReasons$PercentOfTotal = (globalSentReasons[,3]/globalSentReasons[,4])*100

ggplot(globalSentReasons) + aes(y = PercentOfTotal, x = Reason, group = Airline, colour = Airline) + coord_polar() + geom_point() + geom_path() + labs(x = NULL)



