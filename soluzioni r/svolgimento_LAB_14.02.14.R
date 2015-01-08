rm(list=ls())
#importo il dataset

library(mlbench)
data("PimaIndiansDiabetes2")
#"rinomino" per una facilità di lettura
data.frame <- PimaIndiansDiabetes2

#rimuovo i valori na presenti nelle colonne 2:6,valori non ammessi in un knn

for (c in 2:6) {
	valore.medio <- round(mean(data.frame[,c], na.rm=T))
	data.frame[is.na(data.frame[,c]),c] <- valore.medio

}



#suddivisione in training e test in maniera casuale(gli indici vengono
#ordinati per una migliore lettura
index.random <- sort(sample(nrow(data.frame), size=nrow(data.frame)*0.75, replace=F))

#suddivisione in 75-25
data.train <- data.frame[index.random, ]
data.test <- data.frame[-index.random, ]

#verifico se l'ultima colonna è Factor 
str(data.frame)

#effettuo il knn
pred <- knn(data.train[,-9], data.test[,-9], data.train[,9], k=3, prob=F)

#creo una matrice di confusione
conf.matrix <- table (true = data.test[,9], pred)

#verifico l'accuratezza
classAgreement(conf.matrix)$diag