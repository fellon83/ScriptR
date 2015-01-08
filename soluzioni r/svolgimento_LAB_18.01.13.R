#inizializzazione
library(RTextTools)
data(USCongress)
textA <- as.character(USCongress$text[1:50])

#importo libreria tm e acq
library(tm)
data("acq"); corpusB<-acq

#trasformo textA nel corpusA e unisco corpusA e corpusB in corpus
corpusA <- Corpus(VectorSource(textA))
corpus <- c(corpusA,corpusB)

inspect(corpus)

#effettuo le operazioni di pre-processing
corpusC <- tm_map(corpus, content_transformer(tolower)) #tutto minuscolo
corpusC <- tm_map(corpusC, removeNumbers) #rimozione numeri
corpusC <- tm_map(corpusC, removePunctuation) #rimozione punteggiatura
corpusC <- tm_map(corpusC, stripWhitespace) #rimozione spazi aggiuntivi
corpusC <- tm_map(corpusC, removeWords, stopwords("english")) #rimozione stopwords
corpusC <- tm_map(corpusC, stemDocument) #stemming del documento

#verifica del corpus selezionandone solo una parte
inspect(corpusC[1:3])

#creo la matrice documenti-termini e rimuovo i termini con elevata sparsità
dtm <- DocumentTermMatrix(corpusC, control=list(weighting=weightTf))
dtm_final<- removeSparseTerms(dtm, sparse=0.88) #la sparsità è calcolata a tentativi

#verifico i token
show(dtm_final)

#trasformo la matrice doc-term in una matrice dati e aggiungo una colonna di label
data.matrix <- data.frame(inspect(dtm_final))
LABEL <- c(rep("A", 50), rep("B", 50))
data.matrix <- cbind(LABEL, data.matrix)

#carico la libreria e1071
library("e1071")

#ripartizione in training/set casuale 80/20
index.random <-sample(nrow(data.matrix), size=nrow(data.matrix)*0.8, replace=F)
data.matrix.train <- data.matrix[index.random,]
data.matrix.test <- data.matrix[-index.random,]

#training con kernel radiale con gamma 10 e costo slack variable 10
model.radial.svm <- svm(LABEL~.,kernel="radial", gamma=10, cost=10, data=data.matrix.train)
#classifico i documenti nel test set
pred.model<-predict(model.radial.svm, data.matrix.test[,-1])

#creo la matrice di confusione
conf.matrix <- table(true=data.matrix.test[,1], pred=pred.model)

#calcolo l'accuratezza
classAgreement(conf.matrix)$diag

 

