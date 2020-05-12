library('RColorBrewer')
library('NLP')
library("twitteR")
library("tm")
library("SnowballC")
library("wordcloud")
library("xml2")
library("rvest")
library("googleVis")
library("quantmod")

load("twitter_credentials.RData")
setup_twitter_oauth(consumer_key = t.api.key, consumer_secret = t.api.secret,
                    access_token = t.access.token,
                    access_secret = t.access.secret)

# gainers
tweets.GOOG <- searchTwitter('$GOOG', since='2020-04-22', until='2020-04-29', n = 100)
tweets.CACC <- searchTwitter('$CACC', since='2020-04-22', until='2020-04-29', n = 100)
tweets.BKNG <- searchTwitter('$BKNG', since='2020-04-22', until='2020-04-29', n = 100)
tweets.gainers <- c(tweets.GOOG,tweets.CACC,tweets.BKNG)
text.gainers <- lapply(tweets.gainers, function(t) {iconv(t$getText(),"latin1", "ASCII", sub="")})

# losers
tweets.ZM <- searchTwitter('$ZM', since='2020-04-22', until='2020-04-29', n = 100)
tweets.SBAC <- searchTwitter('$SBAC',since='2020-04-22', until='2020-04-29', n = 100)
tweets.AMT <- searchTwitter('$AMT',since='2020-04-22', until='2020-04-29', n = 100)
tweets.losers <- c(tweets.ZM,tweets.SBAC,tweets.AMT)
text.losers <- lapply(tweets.losers, function(t) {iconv(t$getText(),"latin1", "ASCII", sub="")})

# get corpus
getCorpus <- function (tweets) {
  data.source <- VectorSource(tweets)
  data.corpus <- VCorpus(data.source)
  return (data.corpus)
}
corpus.gainers <- getCorpus(text.gainers)
corpus.losers <- getCorpus(text.losers)

# preprocessing
removeURL <- function(x){
  gsub("(http[^ ]*)","",x)
}

removeNumberWords <- function(x){
  gsub("([[:digit:]]+)([[:alnum:]])*","",x)
}

removeStockName <- function(x){
  gsub("(goog)|(cacc)|(bkng)|(zm)|(sbac)|(amt)","",x)
}

getTransCorpus <- function (data.corpus) {
  data.corpus <- tm_map(data.corpus, content_transformer(removeURL))
  data.corpus <- tm_map(data.corpus, content_transformer(removePunctuation))
  data.corpus <- tm_map(data.corpus, content_transformer(tolower))
  data.corpus <- tm_map(data.corpus,content_transformer(removeStockName))
  english.stopwords <- stopwords("en")
  data.corpus <- tm_map(data.corpus,content_transformer(removeWords),english.stopwords)
  data.corpus <- tm_map(data.corpus,content_transformer(removeNumberWords))
  data.corpus <- tm_map(data.corpus,content_transformer(stemDocument))
  data.corpus <- tm_map(data.corpus,content_transformer(stripWhitespace))
  return (data.corpus)
}

Trans.corpus.gainers <- getTransCorpus(corpus.gainers)
Trans.corpus.losers <- getTransCorpus(corpus.losers)

# create the term-document matrix
tdm.gainers <- TermDocumentMatrix(Trans.corpus.gainers) 
tdm.losers <- TermDocumentMatrix(Trans.corpus.losers) 

#  Compare the frequent terms
FFT.gainers <- findFreqTerms(tdm.gainers,lowfreq = 3)
FFT.losers <- findFreqTerms(tdm.losers,lowfreq = 3)

wordFreq.gainers <- rowSums(as.matrix(tdm.gainers))
wordFreq.losers <- rowSums(as.matrix(tdm.losers))

df.wordFreq.gainers <- data.frame(freq = sort(wordFreq.gainers,decreasing = TRUE))
df.wordFreq.losers <- data.frame(freq = sort(wordFreq.losers,decreasing = TRUE))

# wordcloud
palette <- brewer.pal(8,'Dark2')
set.seed(137)
dev.new(width = 1000, height = 1000, unit = "px")
wordcloud(words=names(wordFreq.gainers), freq=wordFreq.gainers, min.freq=3,random.order=F,colors=palette)
dev.off()
dev.new(width = 1000, height = 1000, unit = "px")
wordcloud(words=names(wordFreq.losers), freq=wordFreq.losers, min.freq=3, random.order=F,colors=palette)
dev.off()

# sentiment analysis
# Lexicons
pos.words <- scan('positive-words.txt',
                  what = 'character',
                  comment.char = ';')
neg.words <- scan('negative-words.txt',
                  what = 'character',
                  comment.char = ';')

sentiment <- function(text, pos.words, neg.words){
  text <- gsub('[[:punct:]]','',text)
  text <- gsub('[[:cntrl:]]','',text)
  text <- gsub('\\d+','',text)
  text <- tolower(text)
  # split the text into a vector of words
  words <- strsplit(text,'\\s+')
  words <- unlist(words)
  # find which words are positive
  pos.matches <- match(words, pos.words)
  pos.matches <- !is.na(pos.matches)
  # find which words are negative
  neg.matches <- match(words, neg.words)
  neg.matches <- !is.na(neg.matches)
  # calculate the sentiment score
  p <- sum(pos.matches)
  n <- sum(neg.matches)
  if (p == 0 & n == 0)
    return(NA)
  else
    return(p-n)
}

# calculate score for each tweet
texts.score <- function (text) {
  score <- c()
  for(i in 1:length(text)){
    score[i] <- sentiment(text[i],pos.words, neg.words)
  }
  return(score)
}
gainer.score <- texts.score(text.gainers)  
loser.score <- texts.score(text.losers)

table.gainer.score <- table(gainer.score)
table.loser.score <- table(loser.score)

dev.new(width = 500, height = 500, unit = "px")
par(mfrow=c(1,2))
barplot(table.gainer.score , main="Sentiment Analysis for Gainers",
        xlab="Sentiment Score", ylab="Number of tweets",
        ylim=c(0,100), col="black")
grid(nx=NA,ny=NULL,col=rgb(165,165,165,max=255),lty=1)

barplot(table.loser.score, main="Sentiment Analysis for Losers",
        xlab="Sentiment Score", ylab="Number of tweets",
        ylim=c(0,100), col="black")
grid(nx=NA,ny=NULL,col=rgb(165,165,165,max=255),lty=1)
dev.off()

gainer.vector <- sapply(text.gainers,function (t) {(t)})
loser.vector <- sapply(text.losers,function (t) {(t)})
score.tweets.gainer <- data.frame(Score=gainer.score, Text=gainer.vector)
score.tweets.loser <- data.frame(Score=loser.score, Text=loser.vector)
View(score.tweets.gainer)
View(score.tweets.loser)

mean(gainer.score[is.na(gainer.score)==FALSE])
median(gainer.score[is.na(gainer.score)==FALSE])
mean(loser.score[is.na(loser.score)==FALSE])
median(loser.score[is.na(loser.score)==FALSE])

# Candlestick
getSymbols("GOOG")
getSymbols("CACC")
getSymbols("BKNG")
getSymbols("ZM")
getSymbols("SBAC")
getSymbols("AMT")


plot.Candlestick <- function(stock,head){
  df <- data.frame(Date=index(stock),coredata(stock))
  df <- df[df$Date >= '2020-04-22' & df$Date <= '2020-04-29',]
  Candlestick <- gvisCandlestickChart(df,
                                      xvar = "Date",
                                      low = names(stock)[3],
                                      high = names(stock)[2],
                                      open = names(stock)[1],
                                      close = names(stock)[4],
                                      options=list(legend='none', width=500, height=150,title = head))

}
plot.GOOG <- plot.Candlestick(GOOG,"GOOG")
plot.CACC <- plot.Candlestick(CACC,"CACC")
plot.BKNG <- plot.Candlestick(BKNG,"BKNG")
plot.ZM <- plot.Candlestick(ZM,"ZM")
plot.SBAC <- plot.Candlestick(SBAC,"SBAC")
plot.AMT <- plot.Candlestick(AMT,"AMT")

chart<- gvisMerge(gvisMerge(gvisMerge(plot.GOOG, plot.CACC), plot.BKNG),
                 gvisMerge(gvisMerge(plot.ZM, plot.SBAC), plot.AMT),
                 horizontal = TRUE)

plot(chart)
# cat(chart$html$chart, file = "chart.html")