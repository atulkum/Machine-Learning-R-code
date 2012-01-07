setwd("C:\\mine\\stat202\\project")

cm <- function (actual, predicted)
{
	t<-table(predicted,actual)
	t[apply(t,2,function(c) order(-c)[1]),] 
}

data <- read.csv("training.csv")

data$is_homepage <- as.factor(data$is_homepage)
data$sig9 <- ifelse(data$sig6==0,0,1)
data$sig9<- as.factor(data$sig9)
data$sig3 <- log10(data$sig3 + 1)
data$sig4 <- log10(data$sig4 + 1)
data$sig5 <- round(log10(data$sig5 + 1))
data$sig5 <- as.factor(data$sig5)
data$sig6 <- log10(data$sig6 + 1)

data$sig3 <- round(data$sig3)
data$sig4 <- round(data$sig4)
data$sig6 <- round(data$sig6)

data$sig1 <- round(log10(data$sig1*100 + 1))
data$sig2 <- round(log10(data$sig2*100 + 1))
data$sig7 <- round(log10(data$sig7*100 + 1))
data$sig8 <- round(log10(data$sig8*100 + 1))


N <- nrow(data)
Ntrain <- round(N*0.7)

data <- data[sample(1:N),]
train <- data[1:Ntrain,]
test <- data[(Ntrain+1):N,]

y<-as.factor(train[,13])
x<-train[,3:12]


library(randomForest)

m <- randomForest(y~.,x, ntree=250)
rm(data)
rm(x)
rm(y)
testdata <- read.csv("test.csv")

testdata$is_homepage <- as.factor(testdata$is_homepage)
testdata$sig9 <- ifelse(testdata$sig6==0,0,1)
testdata$sig9<- as.factor(testdata$sig9)
testdata$sig3 <- log10(testdata$sig3 + 1)
testdata$sig4 <- log10(testdata$sig4 + 1)
testdata$sig5 <- round(log10(testdata$sig5 + 1))
testdata$sig5 <- as.factor(testdata$sig5)
testdata$sig6 <- log10(testdata$sig6 + 1)

testdata$sig3 <- round(testdata$sig3)
testdata$sig4 <- round(testdata$sig4)
testdata$sig6 <- round(testdata$sig6)

testdata$sig1 <- round(log10(testdata$sig1*100 + 1))
testdata$sig2 <- round(log10(testdata$sig2*100 + 1))
testdata$sig7 <- round(log10(testdata$sig7*100 + 1))
testdata$sig8 <- round(log10(testdata$sig8*100 + 1))


pred_test <- predict(m, testdata, type = "class")
write(pred_test,"predictions.txt") 



N <- nrow(data)
Ntrain <- round(N*0.7)

data <- data[sample(1:N),]
train <- data[1:Ntrain,]
test <- data[(Ntrain+1):N,]

y<-as.factor(train[,13])
x<-train[,3:12]

y_test <- as.factor(test[,13])
x_test <- test[,3:12]


pred_test <- predict(m,x_test, type = "class")
cm(pred_test, y_test)

pred <- predict(m,x, type = "class")
cm(pred, y)

> cm(pred_test, y_test)
         actual
predicted     0    1
        0 10356 3252
        1  5241 5165
> 
> pred <- predict(m,x, type = "class")
> cm(pred, y)
         actual
predicted     0     1
        0 25086  6365
        1 11300 13281

> 5165/(5165 + 5241)
[1] 0.4963483
> 5165/(5165 + 3252)
[1] 0.6136391
> 13281/(13281 + 11300)
[1] 0.5402954
> 13281/(13281 + 6365)
[1] 0.6760155
