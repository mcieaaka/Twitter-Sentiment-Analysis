#Libraries
library(tidyverse)
library(plotly)
library(maps)
library(rbokeh)
library(widgetframe)
library(htmlwidgets)

#Reading Dataset
library(readxl)
Dataset <- read_excel("D:/sem4/A-SIN/PROJECT/Dataset.xlsx")
View(Dataset)
df<-Dataset
dim(df)

#Counting NAs
apply(df, 2, function(x) sum(is.na(x)))

#Tweets +ve, -ve, neutral
df %>%
  ggplot(aes(airline_sentiment))+
  geom_bar(fill = "tomato", color = "black")+
  labs(x = "Airline Sentiment", y = "Count")

#Tweets with Airlines
df %>%
  ggplot(aes(airline_sentiment))+
  geom_bar(aes(fill = airline))+
  labs(x = "Airline Sentiment", y = "Count", fill = "Airlines")

#No. of tweets per airline
df %>%
  count(airline, airline_sentiment) %>%
  ggplot(aes(airline, n))+
  geom_bar(stat = "identity", colour = "grey19", fill = "skyblue")+
  facet_wrap(~airline_sentiment, ncol = 2)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))+
  labs(x = "Airlines", y = "Count of Tweet Sentiments")

#Count of Sentiments
df %>%
  count(airline, airline_sentiment) %>%
  ggplot(aes(airline_sentiment, n))+
  geom_bar(stat = "identity", colour = "grey19", fill = "skyblue")+
  facet_wrap(~airline, ncol = 2)+
  labs(x = "Airlines", y = "Count of Tweet Sentiments")

#Mean Airline Sentiment
df %>%
  group_by(airline, airline_sentiment) %>%
  summarise(
    mean_airline_sentiment = mean(airline_sentiment_confidence, na.rm = TRUE)
  ) %>%
  ggplot(aes(airline_sentiment, mean_airline_sentiment))+
  geom_bar(stat = "identity", fill = "tomato", color = "black")+
  facet_wrap(~airline, ncol = 2)+
  labs(x = "Airline Sentiment", y = "Mean Airline Sentiment Confidence")

#Reasons of negative without NA's
df %>%
  filter(!is.na(negativereason)) %>%
  ggplot(aes(negativereason))+
  geom_bar(fill = "violetred3", color = "black")+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 9))+
  labs(x = "Negative Reasons", y = "Count of Negative Reasons")

#WRT Airlines
df %>%
  filter(!is.na(negativereason)) %>%
  count(airline, negativereason) %>%
  ggplot(aes(negativereason, n))+
  geom_bar(stat = "identity", colour = "grey19", fill = "springgreen4")+
  facet_wrap(~airline, ncol = 3)+
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 9))+
  labs(x = "Negative Reasons", y = "Count of Negative Reasons")

df %>%
  filter(!is.na(negativereason)) %>%
  count(airline, negativereason) %>%
  ggplot(aes(airline, n))+
  geom_bar(stat = "identity", colour = "grey19", fill = "springgreen4")+
  facet_wrap(~negativereason, ncol = 3)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 9))+
  labs(x = "Airlines", y = "Count of Negative Reasons")

#No. of Retweets
df %>%
  group_by(airline_sentiment) %>%
  summarise(
    number_of_retweets = sum(retweet_count, na.rm = TRUE)
  )

#Reason retweets
df %>%
  filter(airline == "United") %>%
  group_by(negativereason) %>%
  summarise(
    n_retweets = sum(retweet_count, na.rm = TRUE)
  ) %>%
  ggplot(aes(negativereason, n_retweets))+
  geom_bar(stat = "identity", fill = "skyblue", color = "black")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))+
  labs(x = "Negative Reasons", y = "Number of Retweets")

#More Libraries
library(dplyr)
library(tidytext)
library(RColorBrewer)
library(ggplot2)
library(wordcloud)
library(tm)
options(warn=-1)

dataset<-df
str(dataset)

dataset$text <- as.character(dataset$text)
tidy_dataset <- dataset %>%
  unnest_tokens(word, text)

summary(dataset$airline_sentiment)

#Visualization of whether the sentiment of the tweets was positive, neutral, or negative for each airlines
ggplot(dataset, aes(x = airline_sentiment, fill = airline_sentiment)) +
  geom_bar() +
  facet_grid(. ~ airline) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6),
        plot.margin = unit(c(3,0,3,0), "cm"))

#Frequent Words in positive sentiment
positive <- tidy_dataset %>% 
  filter(airline_sentiment == "positive") 

list <- c("to", "the","i", "a", "you", "for", "on", "and", "is", "are", "am", 
          "my", "in", "it", "me", "of", "was", "your", "so","with", "at", "just", "this",
          "http", "t.co", "have", "that", "be", "from", "will", "we", "an", "can")

positive <- positive %>%
  filter(!(word %in% list))

dtm<-as.matrix(positive)
wordcloud(dtm[,15],
          max.words = 100,
          random.order=FALSE, 
          rot.per=0.30, 
          use.r.layout=FALSE, 
          colors=brewer.pal(10, "Blues"))

positive <- positive %>%
  count(word, sort = TRUE) %>%
  rename(freq = n)

head(positive, 21)


positive <- positive %>%
  top_n(21)
colourCount = length(unique(positive$word))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

# The Most 21 Frequent Words in Positive Tweets
positive %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(x = word, y = freq)) +
  geom_col(fill = getPalette(colourCount)) +
  coord_flip() 


#Negative Word Cloud
negative <- tidy_dataset %>% 
  filter(airline_sentiment == "negative") 

negative <- negative %>%
  filter(!(word %in% list))

dtm_n<-as.matrix(negative)
wordcloud(dtm_n[,15],
          max.words = 100,
          random.order=FALSE, 
          rot.per=0.30, 
          use.r.layout=FALSE, 
          colors=brewer.pal(10, "Reds"))

negative <- negative %>%
  count(word, sort = TRUE) %>%
  rename(freq = n)

negative <- negative %>%
  top_n(21)
colourCount = length(unique(negative$word))
getPalette = colorRampPalette(brewer.pal(8, "Dark2"))

# The Most 21 Frequent Words in Negative Tweets
negative %>%
  mutate(word = reorder(word, freq)) %>%
  ggplot(aes(x = word, y = freq)) +
  geom_col(fill = getPalette(colourCount)) +
  coord_flip() 

#Intersection of positive and negative
intersect(negative$word, positive$word)

dataset %>%
  filter(negativereason != "") %>%
  ggplot(aes(x = negativereason)) + 
  geom_bar(fill = "tomato") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))


#Most Frequent in Neutral
neutral <- tidy_dataset %>% 
  filter(airline_sentiment == "neutral") 

neutral <- neutral %>%
  count(word, sort = TRUE) %>%
  rename(freq = n)

neutral <- neutral %>%
  filter(!(word %in% list))

head(neutral, 21)

#Words for each Sentiment
totals <- tidy_dataset %>%
  # Count by tweet id to find the word totals for tweet
  count(tweet_id) %>%
  # Rename the new column
  rename(total_words = n) 


totals <- dataset %>%
  inner_join(totals, by = "tweet_id") %>%
  select(tweet_id, total_words, airline_sentiment) %>%
  arrange(desc(total_words))

totals <- head(totals, 20)

ggplot(totals, aes(x = airline_sentiment , y = total_words, fill = airline_sentiment)) +
  geom_col() +
  scale_fill_brewer(palette="Paired")


#Individual flights
text_df <- data_frame(line = 1:nrow(dataset), airline=dataset$airline, text = dataset$text )

grouped_text <- unnest_tokens(text_df,word, text)
##American wordcloud
grouped_text %>% filter(airline == 'American')%>%
     anti_join(stop_words) %>%
     count(word) %>%
     with(wordcloud(word, n, max.words = 50))

##Delta Wordcloud
grouped_text %>% filter(airline == 'Delta')%>%
     anti_join(stop_words) %>%
     count(word) %>%
     with(wordcloud(word, n, max.words = 50))
##Southwest WordCloud
grouped_text %>% filter(airline == 'Southwest')%>%
     anti_join(stop_words) %>%
     count(word) %>%
     with(wordcloud(word, n, max.words = 50))
##United WordCloud
grouped_text %>% filter(airline == 'United')%>%
     anti_join(stop_words) %>%
     count(word) %>%
     with(wordcloud(word, n, max.words = 50))
##US Airways WordCloud
grouped_text %>% filter(airline == 'US Airways')%>%
     anti_join(stop_words) %>%
     count(word) %>%
     with(wordcloud(word, n, max.words = 50))
##Virgin America Word Cloud
grouped_text %>% filter(airline == 'Virgin America')%>%
     anti_join(stop_words) %>%
     count(word) %>%
     with(wordcloud(word, n, max.words = 20))


#Confidence in negative reason

df %>%
  filter(!is.na(negativereason)) %>%
  group_by(negativereason) %>%
  summarise(
    mean_confidence = mean(negativereason_confidence, na.rm = TRUE)
  ) %>%
  ggplot(aes(negativereason, mean_confidence))+
  geom_bar(stat = "identity", color = "black", fill = "skyblue")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))+
  labs(x = "Negative Reasons", y = "Mean Negative Confidence")

#Negative reason Confidence for airlines
df %>%
  filter(!is.na(negativereason)) %>%
  group_by(airline, negativereason) %>%
  summarise(
    mean_confidence = mean(negativereason_confidence, na.rm = TRUE)
  ) %>%
  ggplot(aes(negativereason, mean_confidence))+
  geom_bar(stat = "identity", colour = "grey19", fill = "springgreen4")+
  facet_wrap(~airline, ncol = 3)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))+
  labs(x = "Negative Reasons", y = "Mean Confidence in Negative Reasons")


