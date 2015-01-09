
#inizializzazione
rm(list=ls())

#Importazione della libreria tm
library(tm)

#Creazione del corpus dal file XML
textA <- system.file("texts", "acq", package="tm")

corpus <- Corpus(DirSource(textA), readerControl = list (reader= readReut21578XMLasPlain))
#visualizzazione dei dati del corpus
corpus



##------------------------------------------------------------#

#Operazione di pre-processing
#lo scopo è quello di ottenere una "bag of words"

#trasformazione del testo tutto in minuscolo
corpusC <- tm_map(corpus, content_transformer(tolower))
#Rimozione dei numeri
corpusC <- tm_map(corpusC, removeNumbers)
#Rimozione della punteggiatura
corpusC <- tm_map(corpusC, removePunctuation)
#rimozione delle stepword (usando il dizionario inglese)
corpusC <- tm_map(corpusC, removeWords, stopwords("english"))
#rimozione degli spazi extra
corpusC <- tm_map(corpusC, stripWhitespace)
#stemming del documento
corpusC <- tm_map(corpusC, stemDocument)

inspect(corpusC[1:3])

##------------------------------------------------------------#

#Costruzione delle matrici Termine-Documenti e Documenti-Termini

tdm<-TermDocumentMatrix(corpusC)
dtm<-DocumentTermMatrix(corpusC)


##------------------------------------------------------------#


#Visualizzazione tramite word cloud dei termini frequenti
library(wordcloud)

#matrice termini-documenti
graphics.off() #chiudo eventuali grafici aperti
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
wordcloud(d$word,d$freq)

#matrice documenti-termini
graphics.off() #chiudo eventuali grafici aperti

m <- as.matrix(dtm)
v <- sort(colSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
wordcloud(d$word,d$freq)


##------------------------------------------------------------#


#Semplificazione della sparsità per entrambe la matrice
#(si procede a tentativi fino al raggiungimento di circa 40termini)

tdm_final <- removeSparseTerms(tdm, sparse=0.79)
dtm_final <- removeSparseTerms(dtm, sparse=0.79)

findFreqTerms(tdm_final)

findFreqTerms(dtm_final)


##------------------------------------------------------------#

#Clustering gerarchico nello spazio dei documenti
#verrà utilizzato la distanza euclidea per il calcolo della dissimilarità
#e metodo del legame completo per il clustering

#Calcolo della dissimilarità
dist<- dist(dtm_final, method="euclidean")

#Clustering gerarchico
hc_complete <- hclust(dist, method="complete")

#Visualizzazione del Dendrogramma con l'aggiunta dei rettangoli 
#per la visualizzazione dei cluster

graphics.off() #chiudo eventuali grafici aperti
plot(hc_complete)
rect.hclust(hc_complete, k=3, border="blue")


##------------------------------------------------------------#

#Flat clustering nello spazio dei termini
#Essendo il flat clustering di tipo probabilistico è opportuno richiamare
#il set.seed affinchè i risultati siano basati sugli stessi dati

set.seed(123)
flat_cluster <- kmeans(tdm_final, 3, iter.max=50)

#visualizzazione di alcuni dati del flat clustering
flat_cluster$cluster
flat_cluster$size
flat_cluster$center


