rm(list=ls())
library(tm)
data("acq")
corpus<-acq[1:20]


#Riporto tutto il testo in minuscolo
corpusC <- tm_map(corpus, content_transformer(tolower))
#Rimuovo numeri
corpusC <- tm_map(corpusC, removeNumbers)
#rimuovo punteggiatura
corpusC <- tm_map(corpusC, removePunctuation)
#rimuovo gli spazi in eccesso
corpusC <- tm_map(corpusC, stripWhitespace)
#Rimuovo le stopwords (uso il dizionario in inglese)
corpusC <- tm_map(corpusC, removeWords, stopwords("english"))
#ed infine procedo allo stemming del documento
corpusC <- tm_map(corpusC, stemDocument)

dtm <- DocumentTermMatrix(corpusC, control=list(weighting =weightTf))

dtm_final <- removeSparseTerms(dtm, sparse=0.85)


findFreqTerms(dtm_final)

dist_euclid <- dist(dtm_final, method="euclidean")
hc_complete <- hclust(dist_euclid, method="complete")


graphics.off() #chiusura di eventuali plot già presenti
plot(hc_complete)
#aggiungo i raggruppamenti
rect.hclust(hc_complete, k=6, border="red")