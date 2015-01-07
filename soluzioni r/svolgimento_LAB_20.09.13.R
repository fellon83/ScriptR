rm(list=ls())

library(RTextTools)
data(USCongress)
textA <- as.character(USCongress$text[101:130])

library(tm)
data("crude")

corpusA <- Corpus(VectorSource(textA))
corpus <- c(corpusA, crude)

corpusC <- tm_map(corpus, content_transformer(tolower)) #tutto minuscolo
corpusC <- tm_map(corpusC, removeNumbers) #rimozione numeri
corpusC <- tm_map(corpusC, removePunctuation) #rimozione punteggiatura
corpusC <- tm_map(corpusC, stripWhitespace) #rimozione spazi aggiuntivi
corpusC <- tm_map(corpusC, removeWords, stopwords("english")) #rimozione stopwords
corpusC <- tm_map(corpusC, stemDocument) #stemming del documento

tdm <- TermDocumentMatrix(corpusC, control=list(weighting=weightTfIdf))
tdm_final <- removeSparseTerms(tdm, sparse = 0.88)

data.frame <- as.data.frame(as.matrix(tdm_final))

dist_euclid <- dist(data.frame, method="euclidean")
dist_block <- dist(data.frame, method="manhattan")

hc_single_euc <- hclust(dist_euclid, method="single")
hc_single_mah <- hclust(dist_block, method="single")

hc_complete_euc <- hclust(dist_euclid, method="complete")
hc_complete_man <- hclust(dist_block, method="complete")

plot(hc_complete_euc)
rect.hclust(hc_complete_euc, k=4, border="blue")

plot(hc_complete_man)
rect.hclust(hc_complete_man, k=4, border="blue")

