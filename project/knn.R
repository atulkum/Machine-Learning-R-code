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

mn <- mean(data$sig1)
sdv <- sd(data$sig1) 
data$sig1 <- (data$sig1 - mn)/sdv
mn <- mean(data$sig2)
sdv <- sd(data$sig2) 
data$sig2 <- (data$sig2 - mn)/sdv
mn <- mean(data$sig3)
sdv <- sd(data$sig3) 
data$sig3 <- (data$sig3 - mn)/sdv
mn <- mean(data$sig4)
sdv <- sd(data$sig4) 
data$sig4 <- (data$sig4 - mn)/sdv
mn <- mean(data$sig6)
sdv <- sd(data$sig6) 
data$sig6 <- (data$sig6 - mn)/sdv
mn <- mean(data$sig7)
sdv <- sd(data$sig7) 
data$sig7 <- (data$sig7 - mn)/sdv
mn <- mean(data$sig8)
sdv <- sd(data$sig8) 
data$sig8 <- (data$sig8 - mn)/sdv

N <- nrow(data)
Ntrain <- round(N*0.7)

data <- data[sample(1:N),]
train <- data[1:Ntrain,]
test <- data[(Ntrain+1):N,]

y<-as.factor(train[,13])
x<-train[,3:12]

y_test <- as.factor(test[,13])
x_test <- test[,3:12]

library(class)

pred <- knn(x,x,y,k=1)
cm(pred, y)

pred <- knn(x,x_test,y,k=1)
cm(pred, y_test)

pred <- knn(x,x,y,k=3)
cm(pred, y)

pred <- knn(x,x_test,y,k=3)
cm(pred, y_test)

pred <- knn(x,x,y,k=5)
cm(pred, y)

pred <- knn(x,x_test,y,k=5)
cm(pred, y_test)


> pred <- knn(x,x,y,k=1)
> cm(pred, y)
         actual
predicted     0     1
        0 25388  6168
        1  9887 14589
> 
> pred <- knn(x,x_test,y,k=1)
> cm(pred, y_test)
         actual
predicted    0    1
        0 9828 3675
        1 5294 5217
> 
> pred <- knn(x,x,y,k=3)
> cm(pred, y)
         actual
predicted     0     1
        0 24891  6665
        1 10714 13762
> 
> pred <- knn(x,x_test,y,k=3)
> cm(pred, y_test)
         actual
predicted    0    1
        0 9972 3531
        1 5259 5252
> 
> pred <- knn(x,x,y,k=5)
> cm(pred, y)
         actual
predicted     0     1
        0 24838  6718
        1 11119 13357
> 
> pred <- knn(x,x_test,y,k=5)
> cm(pred, y_test)
         actual
predicted     0    1
        0 10099 3404
        1  5262 5249

		> 14589/(14589 + 9887)
[1] 0.5960533
> 14589/(14589 + 6168)
[1] 0.7028472
> 5217/(5217 + 5294)
[1] 0.4963372
> 5217/(5217 + 3675)
[1] 0.5867072


> 13762/(13762 + 10714)
[1] 0.5622651
> 13762/(13762 + 6665)
[1] 0.6737162
> 5252/(5252+5259)
[1] 0.499667
> 5252/(5252+3531)
[1] 0.5979734

> 13357/(13357 + 11119)
[1] 0.5457183
> 13357/(13357 + 6718)
[1] 0.6653549
> 5249/(5249 + 5262)
[1] 0.4993816
> 5249/(5249 + 3404)
[1] 0.6066104

