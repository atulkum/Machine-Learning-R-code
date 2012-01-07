setwd("C:\\mine\\stat202\\project")
data <- read.csv("training.csv")

query_length <- data$query_length
is_homepage <- data$is_homepage
is_homepage <- as.factor(data$is_homepage)
sig1 <- data$sig1
sig2 <- data$sig2
sig3 <- data$sig3
sig4 <- data$sig4
sig5 <- data$sig5
sig6 <- data$sig6
sig7 <- data$sig7
sig8 <- data$sig8
relevance <- as.factor(data$relevance)
sig9 <- ifelse(sig6==0,0,1)
sig9<- as.factor(sig9)
sig3 <- log10(sig3 + 1)
sig4 <- log10(sig4 + 1)
sig5 <- round(log10(sig5 + 1))
sig6 <- log2(sig6 + 1)

sig3 <- round(sig3)
sig4 <- round(sig4)
sig6 <- log2(sig6 + 1)

sig1 <- round(log(sig1*100 + 1))
sig2 <- round(log(sig2*100 + 1))
sig7 <- round(log(sig7*100 + 1))
sig8 <- round(log(sig8*100 + 1))


signals <- cbind(query_length,is_homepage, sig1,sig2,sig3,sig4,sig5,sig6,sig7,sig8,sig9, relevance)

library(e1071)
y<-as.factor(signals[,12])
x<-signals[,1:11]

m <- naiveBayes(x, y) 
1-sum(y==predict(m,x))/length(y)

fit<-svm(x,y)
1-sum(y==predict(fit,x))/length(y)


N <- nrow(signals)
Ntrain <- round(N*0.7)

signals <- signals[sample(1:N),]
train <- signals[1:Ntrain,]
test <- signals[(Ntrain+1):N,]



#process data
[1] "query_id"     "url_id"       

x <- rep(18,0)
y <- rep(18,0)
for(i in 1:18){
	xx <- data$relevance[data$query_length == i]
	x[i] <- sum(xx == 1)/length(xx)
	x[i] <- x[i]*100
	y[i] <- length(xx)
}


plot(x, xlab ="Query Length", ylab ="Relevant %",main="Affect of query length on relevancy", type="o", col="blue")


xx <- data$relevance[data$is_homepage == 1]
x <- sum(xx == 1)/length(xx)

xx <- data$relevance[data$is_homepage == 0]
x <- sum(xx == 1)/length(xx)

qplot(sig8,  geom="histogram")
sig8[1:100]

xx <- data$relevance[data$sig1 > 0.5]
sum(xx == 1)/length(xx)

qplot(log10(sig1 * 1000 +1),geom="histogram")

library(rpart)
library(MASS)
library(class)
library(e1071)

N <- nrow(data)
Ntrain <- round(N*0.7)

data <- data[sample(1:N),]
train <- data[1:Ntrain,]
test <- data[(Ntrain+1):N,]

cm <- function (actual, predicted)
{
	# Produce a confusion matrix
	t<-table(predicted,actual)
	# there is a potential bug in here if columns are tied for ordering
	t[apply(t,2,function(c) order(-c)[1]),] 
}


