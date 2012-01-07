setwd("C:\\mine\\stat202\\project")

data <- read.csv("training.csv")
dimnames(data)

[1] "query_id"     "url_id"       "query_length" "is_homepage"  "sig1"         "sig2"         "sig3"         "sig4"        
 [9] "sig5"         "sig6"         "sig7"         "sig8"         "relevance"   

data <- read.csv("training.csv")

data_qlen_1to8  <- data[data$query_length <= 8 & data$query_length >= 1,]
data_qlen_9to18 <- data[data$query_length <= 18 & data$query_length >= 9,]

 
plot(data$query_length, type="o", col="blue")

qplot(data$sig1, data=data, geom="histogram")

qplot(round(log(data$sig6 + 1)), data=data, geom="histogram")
nrow(data)
qlen_prob <- rep(18,0)
qlenngth <- rep(18,0)
for(i in 1:18){
	qlen <- data$relevance[data$query_length == i & data$is_homepage == 1]
	qlen_prob[i] <- sum(qlen == 0)/length(qlen)
	qlenngth[i] <- length(qlen)
}


plot(qlen_prob, type="o", col="blue")

hmpge0 <- data$relevance[data$is_homepage == 0]
hmpge1 <- data$relevance[data$is_homepage == 1]

error0 <- sum(hmpge0 == 0)/length(hmpge0)
error1 <- sum(hmpge1 == 0)/length(hmpge1)
	
length(hmpge0)
length(hmpge1)

> error0
[1] 0.581052
> error1
[1] 0.5136102

data_qlen_1to8  <- data[data$query_length <= 8 & data$query_length >= 1, 1:13]
data_qlen_9to18 <- data[data$query_length <= 18 & data$query_length >= 9, 1:13]

library(e1071)
x <- data_qlen_1to8[,3:12]
y <-data_qlen_1to8[,13]
fit<-svm (x,y,kernel="linear",cost=100000)

train_error <- 1-sum(y==predict(fit,x))/length(y)


