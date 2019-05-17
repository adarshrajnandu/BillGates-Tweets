# BillGates-Tweets
Perform sentimentimental analysis on the tweets by Bill Gates
library(twitteR)
library(ROAuth)
library(base64enc)
library(httpuv)




cred <- OAuthFactory$new(consumerKey="8Szm3rFQE03E#############",
                         consumerSecret="hCSNvOeMe######KaiqH5pttGs########################",
                         requestURL="https://api.twitter.com/oauth/request_token",
                         accessURL="https://api.twitter.com/oauth/access_token",
                         authURL="https://api.twitter.com/oauth/authorize")



save(cred, file="twitter authentication.Rdata")
load("twitter authentication.Rdata")




setup_twitter_oauth("8Szm3rFQE03E#############", 
                    "hCSNvOeMe######KaiqH5pttGs########################",
                    "2524690621-WMRvT4oJJ9W8AKj$######$#$#$##########$#", ##### Access token
                    "i54rAUppXa9qpanc60IJ#$%$##$$%%%$###$%%%$##@@#")      ##### Access token secret key

Tweets <- userTimeline('BillGAtes', n = 1000)

TweetsDF <- twListToDF(Tweets)
write.csv(TweetsDF, "BillGates.csv")




BillGates_tweets <- searchTwitter('BillGates', n=1000, lang="en", resultType = 'recent')
class(BillGates_tweets)

BillGates_tweets[1:20]



BillGates_txt <- sapply(BillGates_tweets, function(x) x$getText())
str(BillGates_txt)


library(tm)  
library(SnowballC)

BillGates_corpus <- Corpus(VectorSource(BillGates_txt))
inspect(BillGates_corpus[10])


library(tidyverse)

BillGates_clean <- tm_map(BillGates_corpus, removePunctuation)
BillGates_clean <- tm_map(BillGates_clean, content_transformer(tolower))
BillGates_clean <- tm_map(BillGates_clean, removeWords, stopwords("english"))
BillGates_clean <- tm_map(BillGates_clean, removeNumbers)
BillGates_clean <- tm_map(BillGates_clean, stemDocument)  
BillGates_clean <- tm_map(BillGates_clean, stripWhitespace)


inspect(BillGates_clean[3])
class(BillGates_clean)




######## term document matrix
tdm <- TermDocumentMatrix(BillGates_clean)
tdm
tdm_rmsparse <- removeSparseTerms(x = tdm, sparse = 0.98 )
tdm_rmsparse
tdm <- as.matrix(tdm_rmsparse)
tdm[1:20,1:20]

v <- sort(rowSums(tdm), decreasing=T)
head(v)



s <- rowSums(tdm)
s_sub <- subset(s,s  > 10)
barplot(s_sub,col = rainbow(10),las = 3)

library(wordcloud)
windows()
wordcloud(words =  BillGates_clean,random.order = F,col = rainbow(30),random.color = F,
          max.freq =80,scale = c(3,1),rot.per = 0.5,max.words =100 )

BillGates_clean <- tm_map(BillGates_clean, removeWords, c("...","billgat","sapink"))



#emotion mining


install.packages("syuzhet")
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(dplyr)
library(reshape2)

txt = readLines(file.choose())
txt <- iconv(txt, "UTF-8")
?iconv
x <- get_nrc_sentiment(txt)
head(x,n=20)


example<-get_sentences(txt)
nrc_data<-get_nrc_sentiment(example)



# Bar plot for emotion mining
windows()
barplot(colSums(nrc_data), las = 2, col = rainbow(10), ylab = 'Count', main = 'Emotion scores')



sentiment_nrc<-get_sentiment(example,method="nrc")


# Most Negative and Positive tweet
negative<-example[which.min(sentiment_vector)]
positive<-example[which.max(sentiment_vector)]

