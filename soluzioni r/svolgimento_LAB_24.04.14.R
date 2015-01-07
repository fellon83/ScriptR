library("mlbench")
data("Glass")
class(Glass)
data.matrix <- Glass;

library("e1071")

#ripartizione in training/set casuale 80/20
index.random <-sample(nrow(data.matrix), size=nrow(data.matrix)*0.8, replace=F)
data.matrix.train <- data.matrix[index.random,]
data.matrix.test <- data.matrix[-index.random,]

#training con kernel radiale e 10-cross validation
model.radial.svm <- tune.svm(Type~.,kernel="radial", gamma=10^(-6:-1), cost=10^(1:2), data=data.matrix.train)
final.model <- svm(Type~., kernel="radial", data=data.matrix.train, gamma=0.1, cost=10)

pred.final.model <- predict(final.model, data.matrix.test[,-1]) #????
conf.matrix <- table(true=data.matrix.test[,1], pred= pred.final.model)

classAgreement(conf.matrix)$diag

