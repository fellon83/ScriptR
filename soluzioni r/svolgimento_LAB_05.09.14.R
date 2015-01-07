rm(list=ls())
library(tm)
data("acq")
corpus <- acq[1:30]

#effettuo le operazioni di pre-processing
corpus <- tm_map(corpus, content_transformer(tolower)) #tutto minuscolo
corpus<- tm_map(corpus, removeNumbers) #rimozione numeri
corpus<- tm_map(corpus, removePunctuation) #rimozione punteggiatura
corpus <- tm_map(corpus, stripWhitespace) #rimozione spazi aggiuntivi
corpus <- tm_map(corpus, removeWords, stopwords("english")) #rimozione stopwords
corpus <- tm_map(corpus, stemDocument) #stemming del documento

tdm <- TermDocumentMatrix(corpus, contro=list(weighting=weightTf))
tdm_final<- removeSparseTerms(tdm, sparse=0.79) #la sparsità è calcolata a tentativi

#i termini frequenti saranno:
findFreqTerms(tdm_final)

#effettuo un clustering gerarchico con dissimilarità calcolata con distanza euclidea e linkage completo

dist_euclid <- dist(tdm_final, method="euclidean")
hc_complete <- hclust(dist_euclid, method="complete")

#Costruisco il dendrogramma
graphics.off() #chiusura di eventuali plot già presenti
plot(hc_complete)
#aggiungo i raggruppamenti
rect.hclust(hc_complete, k=6, border="red")

tdm <- TermDocumentMatrix(corpus, contro=list(weighting=weightTfIdf))
tdm_final<- removeSparseTerms(tdm, sparse=0.79) #la sparsità è calcolata a tentativi

#i termini frequenti saranno:
findFreqTerms(tdm_final)

#effettuo un clustering gerarchico con dissimilarità calcolata con distanza euclidea e linkage completo

dist_euclid <- dist(tdm_final, method="euclidean")
hc_complete <- hclust(dist_euclid, method="complete")

#Costruisco il dendrogramma
plot(hc_complete)
#aggiungo i raggruppamenti
rect.hclust(hc_complete, k=6, border="red")
