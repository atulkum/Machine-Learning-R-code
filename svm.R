setwd("C:\\mine\\stat202\\hw5")
library(e1071)
train<-read.csv("sonar_train.csv",header=FALSE)
y<-as.factor(train[,61])
x<-train[,1:60]

fit<-svm(x,y)
train_error <- 1-sum(y==predict(fit,x))/length(y)

test<-read.csv("sonar_test.csv",header=FALSE)
y_test<-as.factor(test[,61])
x_test<-test[,1:60]
test_error <- 1-sum(y_test==predict(fit,x_test))/length(y_test)
