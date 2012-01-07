setwd("C:\\mine\\stat202\\hw5")
train<- read.csv("sonar_train.csv",header=FALSE)
test<- read.csv("sonar_test.csv",header=FALSE)
y<- train[,61]
x<- train[,1:60]
y_test<- test[,61]
x_test<- test[,1:60]
train_error<- rep(0,500) # Keep track of errors
test_error<- rep(0,500)
f<- rep(0,130) # 130 pts in training data
f_test<- rep(0,78) # 78 pts in test data
i<- 1
library(rpart)
while(i <= 500){
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
  train_error[i] <- sum(1*f*y<0)/130
  test_error[i] <- sum(1*f_test*y_test<0)/78
  i<-i+1
}

plot(seq(1,500),test_error,type="l",
   ylim=c(0,.5),
   ylab="Error Rate",xlab="Iterations",lwd=2, main='Atul Kumar')
lines(train_error,lwd=2,col="purple")
legend(4,.5,c("Training Error","Test Error"),   
    col=c("purple","black"),lwd=2)

