#inizializzazione
rm(list=ls())
library(RTextTools)
data(USCongress)
text.vector <- as.character(USCongress$text[1:150])

#carico la libreria tm
library(tm)
corpus <- Corpus(VectorSource(text.vector))
#verifico che tutto sia andato per il meglio
inspect(corpus[1:3])

#effettuo le operazioni di preprocessing tramite tm_map
#della libreria tm in modo tale da ottenere una "Bag of words"

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

#verifico il corpus
inspect(corpusC[1:3])

#costruisco la matrice termini-Documenti
tdm <- TermDocumentMatrix(corpusC, control=list(weighting =weightBin))

#verifico la matrice termine-documenti
show(tdm)

#riduco la sparsità dei termini che non sono presenti nel 90%
tdm_final <- removeSparseTerms(tdm, sparse=0.90)

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



