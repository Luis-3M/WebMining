closeAllConnections()
rm(list=ls())
#R Packages
library(rvest)
library(arules)
library(XML)
library(dplyr)
library(wordcloud)
library(tm)
source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")

query <- "tt0120737"
# Store web url
imdbURL <- "http://www.imdb.com/title/"
movieURL<-paste(imdbURL,query,sep="")
movieURL
site <- html_session(movieURL)

#Scrape the website for the movie rating
n<- site %>% 
  html_node(".imdbRating a") %>%
  html_text(trim=T)

aux<-gsub(",", ".", gsub("\\.", "", n))

number_of_reviews<- as.numeric(gsub("\\.","", as.character(aux)))

reviewsLink <- paste(movieURL,"/reviews?count=",sep="")
reviewsLink2 <- paste(reviewsLink,number_of_reviews,sep="")
reviewsLink3<-  paste(reviewsLink2,"&start=0",sep="")

##### Obtain reviews #########
rev <- read_html(reviewsLink3)
review <- rev %>% 
  html_nodes("#tn15content p") %>%
  html_text(trim=T)
review[23]
##### Obtain rating #########
rat <- read_html(reviewsLink3)
rating <- rat %>% 
  html_nodes(xpath="//div/div/img/@alt") %>%
  html_text(trim=T)
head(rating,8)

####### Text Mining #######
docs <- Corpus(VectorSource(review))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
rem <- function(x) gsub('[[:space:]|[:punct:]]+', ' ', x)
docs <- tm_map(docs, content_transformer(rem))
rem <- function(x) gsub('<[a-z]*>', '', x)
docs <- tm_map(docs, content_transformer(rem))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, toSpace, "<[a-z]*>")
docs <- tm_map(docs, toSpace, "[[a-z0-9]*]")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, stemDocument)

# Matrix
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
#Top 20 Most Used Words
head(d,20)

findFreqTerms(dtm, lowfreq = 2000)
freq.terms <- findFreqTerms(dtm, lowfreq = 2500)
term.freq <- rowSums(as.matrix(dtm))
term.freq <- subset(term.freq, term.freq >= 2000)
df <- data.frame(term = names(term.freq), freq = term.freq)

barplot(d[1:20,]$freq, las = 2, names.arg = d[1:20,]$word,
        col ="lightblue", main ="Most Frequent Words",
        ylab = "Word Frequencies")

plot(dtm, term = freq.terms, corThreshold = 0.30, weighting = T)

# Word Cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 2000, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))

# Words Correlation
findAssocs(dtm, terms = "good", corlimit = 0.3)
findAssocs(dtm, terms = "great", corlimit = 0.2)