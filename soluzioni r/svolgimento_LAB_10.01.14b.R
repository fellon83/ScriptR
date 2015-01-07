rm(list=ls())
library(tm)
crude <- system.file("texts", "crude", package="tm")
corpusB <- Corpus(DirSource(crude), readerControl= list(reader=readReut21578XMLasPlain))

data("acq")
corpusA <- acq

corpus <- c(corpusA, corpusB)

corpusC <- tm_map(corpus, content_transformer(tolower))
corpusC <- tm_map(corpusC, removeNumbers)
corpusC <- tm_map(corpusC, removePunctuation)
corpusC <- tm_map(corpusC, stripWhitespace)
corpusC <- tm_map(corpusC, removeWords, stopwords("english"))
corpusC <- tm_map(corpusC, stemDocument)

tdm <- TermDocumentMatrix(corpusC)
tdm_final <- removeSparseTerms(tdm, sparse = 0.7)
tdm_final 
m1 <- as.matrix(tdm_final)

m1


prcomp(m1, center=F)$rotation[,1:2]

svd(m1)$v[,1:2]

docM1 <- svd(m1)$u %*% diag(c(svd(m1)$d[1:2], rep(0,11))) %*% t(svd(m1)$v)
library(corrplot)
corrplot(cor(docM1))

