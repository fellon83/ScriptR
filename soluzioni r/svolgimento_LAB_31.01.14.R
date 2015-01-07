#inizializzazione
rm(list=ls())

#importo il dataset
library("mlbench")
data(Vehicle)
data.frame <- Vehicle

#suddivisione dataframe in training set (80%) e test set(20%)
index.random <- sort(sample(nrow(data.frame), size=nrow(data.frame)*0.8, replace=F))
data.frame.train <- data.frame[index.random, ]
data.frame.test <- data.frame[-index.random, ]

#svm lineare
library(e1071)
data.model <- svm (Class~., data=data.frame.train, kernel="linear", gamma=1)

pred.model <- predict(data.model, data.frame.test)

#matrice di confusione e accuratezza
conf.matrix <- table(true=data.frame.test[,19], pred= pred.model)
classAgreement(conf.matrix)$diag


