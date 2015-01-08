#inizializzazione
rm(list=ls())
library(RTextTools)
data(USCongress)
textA <- as.character(USCongress$text[51:150])
library(tm)
data("acq")
corpusB<-acq

#trasformo textA nel corpus A e creo un unico corpus unendolo a corpusB
corpusA<-Corpus(VectorSource(textA))
corpus<-c(corpusA,corpusB)
corpus

#pre-processing
corpusC <- tm_map(corpus, content_transformer(tolower))
corpusC <- tm_map(corpusC, removeNumbers)
corpusC <- tm_map(corpusC, removePunctuation)
corpusC <- tm_map(corpusC, removeWords, stopwords("english"))
corpusC <- tm_map(corpusC, stripWhitespace)
corpusC <- tm_map(corpusC, stemDocument)

tdm<- DocumentTermMatrix(corpusC, control=list(weighting=weightBin))
tdm_final<- removeSparseTerms(tdm, sparse=0.88)


#trasformo la matrice Documenti-termini in matrice dati
data.matrix <- data.frame(inspect(tdm_final))
LABEL <- c(rep("A", 100), rep("B", 50))
data.matrix <- cbind(LABEL, data.matrix)

#divido i documenti in test-set 60-40

#creo un array di indici casuali e gli ordino in maniera crescente per una migliore leggibilità
index.random <- sort(sample(nrow(data.matrix), size=nrow(data.matrix)*0.6, replace=F))
data.train <- data.matrix[index.random, ]
data.test <- data.matrix[-index.random, ]

#importo la libreria e1071 ed effettuo il training tramite svm
library(e1071)
model <- svm(LABEL~., data=data.train, kernel="radial", gamma=10, cost=5)
pred.model <- predict(model, data.test[,-1])

#creo la matrice di confusione e ne testo l'accuratezza
conf.matrix <- table (true=data.test[,1], pred=pred.model)
classAgreement(conf.matrix)$diag
