library(readxl)
Dataset <- read_excel("D:/sem4/A-SIN/PROJECT/Dataset.xlsx")
View(Dataset)

data<-Dataset
library(dplyr)

positive = subset(data, airline_sentiment == 'positive')
negative = subset(data, airline_sentiment == 'negative')
library(NLP);
library(tm); 
library(SnowballC);
wordsToRemove = c('get', 'cant', 'can', 'now', 'just', 'will', 'dont', 'ive', 'got', 'much')
analyseText = function(text_to_analyse){
  text_to_analyse  = iconv(text_to_analyse, to = "utf-8") 
  CorpusTranscript = Corpus(VectorSource(text_to_analyse))
  CorpusTranscript = tm_map(CorpusTranscript, tolower)
  CorpusTranscript = tm_map(CorpusTranscript, removeNumbers)
  CorpusTranscript = tm_map(CorpusTranscript, removePunctuation)
  CorpusTranscript = tm_map(CorpusTranscript, removeWords, wordsToRemove)
  CorpusTranscript = tm_map(CorpusTranscript, removeWords, stopwords("english"))
  CorpusTranscript = TermDocumentMatrix(CorpusTranscript)
  CorpusTranscript = removeSparseTerms(CorpusTranscript, 0.97) # keeps a matrix 97% sparse
  CorpusTranscript = CorpusTranscript[names(tail(sort(rowSums(as.matrix(CorpusTranscript))), 50)), ]
  
  return(CorpusTranscript)
}

words_neg = analyseText(negative$text)
words_pos = analyseText(positive$text)

d = dist(t(as.matrix(words_neg)), method = 'euclidean')
fit = hclust(d = d, method = 'ward.D')

#fancy plot
op = par(bg = "#DDE3CA")
plot(fit, col = "#487AA1", col.main = "#45ADA8", col.lab = "#7C8071", main = 'Negative Sentiment', xlab = '',
     col.axis = "#F38630", lwd = 3, lty = 3, sub = "", hang = -1, axes = FALSE)
# add axis
axis(side = 2, at = seq(0, 400, 100), col = "#F38630", labels = FALSE, 
     lwd = 2)
# add text in margin
mtext(seq(0, 100, 10), side = 2, at = seq(0, 100, 10), line = 1, 
      col = "#A38630", las = 2)

plot.new()
plot(fit, hang=-1, main = 'Negative Sentiment', xlab = '')
rect.hclust(fit, k=4, border="red")


# positive sentiment tweets
d = dist(t(as.matrix(words_pos)), method = 'euclidean')
fit = hclust(d = d, method = 'ward.D')

#fancy plot
op = par(bg = "#DDE3CA")
plot(fit, col = "#487AA1", col.main = "#45ADA8", col.lab = "#7C8071", main = 'Positive Sentiment', xlab = '',
     col.axis = "#F38630", lwd = 3, lty = 3, sub = "", hang = -1, axes = FALSE)
# add axis
axis(side = 2, at = seq(0, 400, 100), col = "#F38630", labels = FALSE, 
     lwd = 2)
# add text in margin
mtext(seq(0, 100, 10), side = 2, at = seq(0, 100, 10), line = 1, 
      col = "#A38630", las = 2)
plot.new()
plot(fit, hang=-1, main = 'Positive Sentiment', xlab = '')
rect.hclust(fit, k=4, border="red")
