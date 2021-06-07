library(readxl)
Dataset <- read_excel("D:/sem4/A-SIN/PROJECT/Dataset.xlsx")
View(Dataset)

data<-Dataset

library(tidyverse)
library(tm)
library(NLP)
tweet_data <- subset(data, airline_sentiment != 'neutral')
tweet_data <- subset(tweet_data, select=c('airline_sentiment', 'text'))
head(tweet_data)

dt <- sort(sample(nrow(tweet_data), nrow(tweet_data)*.7))
train<-tweet_data[dt,]
test<-tweet_data[-dt,]

train$airline_sentiment <- as.factor(train$airline_sentiment)

corpus <- VCorpus(VectorSource(c(train$text, test$text))) #save text into corpus

corpus <- tm_map(corpus, content_transformer(tolower)) # transfor to low case
corpus <- tm_map(corpus, PlainTextDocument, lazy = T) # creat a plain text document
corpus <- tm_map(corpus, removePunctuation)  # remove punctuation
corpus <- tm_map(corpus, removeWords, stopwords(kind = "english")) # remove stop words
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, stemDocument)


dtm <- DocumentTermMatrix(corpus)

sparse <- removeSparseTerms(dtm, 0.97) # keep terms that appear in at least 1% of the documents

important_words_df <- as.data.frame(as.matrix(sparse))
colnames(important_words_df) <- make.names(colnames(important_words_df))


important_words_train_df <- head(important_words_df, nrow(train))
important_words_test_df <- tail(important_words_df, nrow(test))


# Add to original dataframes
train_data_words_df <- cbind(train, important_words_train_df)
test_data_words_df <- cbind(test, important_words_test_df)

# Get rid of the original Text field
train_data_words_df$text <- NULL
test_data_words_df$text <- NULL

# train logistic model based in training data
log_model <- glm(airline_sentiment~., data=train_data_words_df, family=binomial)
summary(log_model)
# use our model on test data 
log_pred <- predict(log_model, newdata=test_data_words_df, type="response")
log_pred[1:5]
# compare the predicted result and actual class
table(test_data_words_df$airline_sentiment, log_pred>.5)

# train naive bayes model 
library(e1071)
library(devtools)
library(usethis)

naive_model <- naiveBayes(airline_sentiment~., data = train)
naive_predict <- predict(naive_model, test[,-1])
table(naive_predict, true = test_data_words_df$airline_sentiment)


naive_model1 <- naiveBayes(airline_sentiment~., data = train)
naive_predict1 <- predict(naive_model, test)
table(naive_predict, true = test$airline_sentiment)

library(fpc)
positive <- subset(tweet_data, airline_sentiment=='positive' )
negative <- subset(tweet_data, airline_sentiment=='negative')
splitdata = function(text_to_analyse){
  corpus <- VCorpus(VectorSource(text_to_analyse)) #save text into corpus
  
  corpus <- tm_map(corpus, content_transformer(tolower)) # transfor to low case
  corpus <- tm_map(corpus, PlainTextDocument, lazy = T) # creat a plain text document
  corpus <- tm_map(corpus, removePunctuation)  # remove punctuation
  corpus <- tm_map(corpus, removeWords, stopwords(kind = "english")) # remove stop words
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, stemDocument)
  dtm <- DocumentTermMatrix(corpus)
  sparse <- removeSparseTerms(dtm, 0.97)
  important_words_df <- as.data.frame(as.matrix(sparse))
  colnames(important_words_df) <- make.names(colnames(important_words_df))
  
  return(important_words_df)
}

neg_word = splitdata(negative$text)
# calculate the distance of each word
distance_neg <- dist(t(as.matrix(neg_word)), method = 'euclidean')
kmodel <- kmeans(distance_neg, 3)
kmodel
# plot the cluster
library(cluster)
clusplot(as.matrix(distance_neg), kmodel$cluster, color = T, shade = T, labels = 2, lines = 2, cex = 0.4)


pos_word <- splitdata(positive$text)
distance_pos <- dist(t(as.matrix(pos_word)), method = 'euclidean')
kmodel_pos <- kmeans(distance_pos, 3)
kmodel_pos
# plot the cluster
clusplot(as.matrix(distance_pos), kmodel_pos$cluster, color = T, shade = T, labels = 2, lines = 2, cex = 0.4)