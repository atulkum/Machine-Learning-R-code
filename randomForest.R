setwd("C:\\mine\\stat202\\hw5")
library(randomForest)
train<-read.csv("sonar_train.csv",header=FALSE)
y<-as.factor(train[,61])
x<-train[,1:60]
fit<-randomForest(x,y)
error <- 1-sum(y==predict(fit,x))/length(y)
