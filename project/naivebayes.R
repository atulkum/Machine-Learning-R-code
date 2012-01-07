setwd("C:\\mine\\stat202\\project")

cm <- function (actual, predicted)
{
	t<-table(predicted,actual)
	t[apply(t,2,function(c) order(-c)[1]),] 
}

data <- read.csv("training.csv")

query_length <- data$query_length
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

y_test <- as.factor(test[,13])
x_test <- test[,3:12]

library(e1071)

m <- naiveBayes(x, y) 

pred_test <- predict(m,x_test, type = "class")
cm(pred_test, y_test)

pred <- predict(m,x, type = "class")
cm(pred, y)

> cm(pred_test, y_test)
         actual
predicted    0    1
        0 9667 3889
        1 5116 5342
> 
> pred <- predict(m,x, type = "class")
> cm(pred, y)
         actual
predicted     0     1
        0 22311  9192
        1 11985 12544
> 
> 5342/(5342 + 5116)
[1] 0.5108051
> 5342/(5342 + 3889)
[1] 0.5787022
> 12544/(12544 + 11985)
[1] 0.5113947
> 12544/(12544 + 9192)
[1] 0.5771071
> 
