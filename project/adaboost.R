setwd("C:\\mine\\stat202\\project")

cm <- function (actual, predicted)
{
	t<-table(predicted,actual)
	t[apply(t,2,function(c) order(-c)[1]),] 
}
mydata <- read.csv("training.csv")

query_length <- mydata$query_length
mydata$is_homepage <- as.factor(mydata$is_homepage)
mydata$sig9 <- ifelse(mydata$sig6==0,0,1)
mydata$sig9<- as.factor(mydata$sig9)
mydata$sig3 <- log10(mydata$sig3 + 1)
mydata$sig4 <- log10(mydata$sig4 + 1)
mydata$sig5 <- round(log10(mydata$sig5 + 1))
mydata$sig5 <- as.factor(mydata$sig5)
mydata$sig6 <- log10(mydata$sig6 + 1)

mydata$sig3 <- round(mydata$sig3)
mydata$sig4 <- round(mydata$sig4)
mydata$sig6 <- round(mydata$sig6)

mydata$sig1 <- round(log10(mydata$sig1*100 + 1))
mydata$sig2 <- round(log10(mydata$sig2*100 + 1))
mydata$sig7 <- round(log10(mydata$sig7*100 + 1))
mydata$sig8 <- round(log10(mydata$sig8*100 + 1))
N <- nrow(mydata)
Ntrain <- round(N*0.7)

mydata <- mydata[sample(1:N),]
train <- mydata[1:Ntrain,]
test <- mydata[(Ntrain+1):N,]

y<-train[,13]*2 - 1
x<-train[,3:12]

y_test <- test[,13]*2 -1
x_test <- test[,3:12]

f<- rep(0,nrow(x)) 
f_test<- rep(0,nrow(x_test)) 
i<- 1
library(rpart)
while(i <= 100){
  w <- exp(-y*f) # This is a shortcut to compute w
  w <- w/sum(w)
  fit <- rpart(y~., x, w, method="class")
  g<- -1 + 2*(predict(fit,x)[,2]>.5) # make -1 or 1
  g_test <- -1+2*(predict(fit,x_test)[,2]>.5)
  e <- sum(w*(y*g<0))
  alpha <- .5*log ( (1-e) / e )
  alpha <- 0.1*alpha #change made for part b
  f <- f + alpha*g
  f_test <- f_test + alpha*g_test
  i<-i+1
}
f_test <- ifelse(f_test*y_test > 0, y_test,-y_test)
cm(f_test, y_test)

f <- ifelse(f*y > 0, y,-y)
cm(f, y)

cm(f_test, y_test)
         actual
predicted    -1    1
       -1 11113 2386
       1   6060 4455
> 
> f <- ifelse(f*y > 0, y,-y)
> cm(f, y)
         actual
predicted    -1     1
       -1 26047  5513
       1  14155 10317

	   > 4455/(6060 + 4455)
[1] 0.4236805
> 4455/(6060 + 2386)
[1] 0.5274686
> 10317/(14155 + 10317)
[1] 0.4215839
> 10317/(5513 + 10317)
[1] 0.6517372
