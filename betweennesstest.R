library(readxl)
Dataset <- read_excel("D:/sem4/A-SIN/PROJECT/Dataset.xlsx")
View(Dataset)

data<-Dataset


#Building and Cleaning Corpus
library(NLP)
library(tm)
corpus <- iconv(data$text, to = "utf-8") 
corpus <- Corpus(VectorSource(corpus))
corpus <- tm_map(corpus,tolower)
corpus <- tm_map(corpus,removePunctuation) 
corpus <- tm_map(corpus, removeNumbers) 
cleanset <-  tm_map(corpus,removeWords,stopwords('english'))
removeURL <- function(x) gsub('http[[:alnum:]]*','', x)
cleanset <- tm_map(cleanset,content_transformer(removeURL))
cleanset <- tm_map(cleanset,removeWords,c('corona','covid','positive'))
cleanset <- tm_map(cleanset, stripWhitespace)

#TDM
tdm <- TermDocumentMatrix(cleanset)
t<- removeSparseTerms(tdm,sparse =0.98)
t
t <- as.matrix(t)

#WordCloud Combined
library(RColorBrewer) 
library(wordcloud)
w <- sort(rowSums(t), decreasing =TRUE)
set.seed(222)
wordcloud(words =names(w), freq = w,
          max.words = 150,
          random.order = F,
          min.freq = 5,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5, 0.3),
          rot.per = 0.7)

#Sentiment
library(syuzhet) 
library(lubridate) 
library(ggplot2) 
library(scales) 
library(reshape2) 
library(dplyr)

s<-get_nrc_sentiment(data$text)
head(s)

barplot(
    colSums(s),
    las = 2,
    col = rainbow(ncol(s))
)

#network of Terms
library(igraph) 
tdm[tdm>1]<-1 
termM<- as.matrix(tdm) %*% as.matrix(t(tdm))
g<-graph.adjacency(termM, weighted = T, mode = 'undirected') 
g
g<-simplify(g) #same terms apearing many times will be removed 
V(g)$label<-V(g)$name
V(g)$degree<-degree(g)



#Histogram of node degree
hist(V(g)$degree, 
     breaks = 100 , 
     col = "green",
     main = "Histogram of node Degree" , 
     ylab = "Frequency" ,
     xlab = "Degree of vertices" )

#network dia
set.seed(222)
plot(g)
plot(g,vertex.color = "orange" , vertex.size = 6 , vertex.label.dist = 1.5 ,vertex.label = NA) # without lable

#CommunityDetection
#Edge BetweenNess
#comm <- cluster_edge_betweenness(g)
#plot(comm,g)

#Using propagative lables 
prop <- cluster_label_prop(g) 
plot(prop,g)

#greedy optimization
#greed <- cluster_edge_betweenness(as.undirected(g)) 
#plot(greed, as.undirected(g))
C1<-cluster_fast_greedy(g)
plot(C1,g)


C2<-estimate_edge_betweenness(g,cutoff = 10)
summary(C2)
C3<-C2[C2<max(C2)]
plot(C3)

