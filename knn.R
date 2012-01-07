setwd("C:\\mine\\stat202\\hw5")

train<-read.csv("sonar_train.csv",header=FALSE)
y<-as.factor(train[,61])
x<-train[,1:60]
		
test<-read.csv("sonar_test.csv",header=FALSE)
y_test<-as.factor(test[,61])
x_test<-test[,1:60]

train_error <- rep(0, 10)
test_error <- rep(0, 10)

library(class)

for(i in 1:10){
	fit<-knn(x,x,y,k=i)
	train_error[i] <- 1-sum(y==fit)/length(y)
	
	fit_test<-knn(x,x_test,y,k=i)
	test_error[i] <- 1-sum(y_test==fit_test)/length(y_test)	
}

plot(train_error, type="o", col="blue", ylim=c(0,0.5), xlab='K', ylab='Error', main='Atul Kumar')
lines(test_error, type="o", pch=22, lty=2, col="red", ylim=c(0,0.5))
legend(1,0.50,c("Training Error","Test Error"), col=c("blue","red"),lwd=c(1,4))
