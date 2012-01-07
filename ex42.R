setwd("C:\\mine\\stat202\\hw6")

data<-read.csv("sonar_test.csv",header=FALSE)
x<-data[,1:2,]
plot(x,pch=19,xlab=expression(x[1]), ylab=expression(x[2]))
fit<-kmeans(x, 2)
points(fit$centers,pch=19,col="blue",cex=2)
points(x,col=fit$cluster,pch=19)

data<-read.csv("sonar_test.csv",header=FALSE)
x<-data[,1:2,]
y<-data[,61]
1-sum(fit$cluster*2-3==y)/length(y)

data<-read.csv("sonar_test.csv",header=FALSE)
x<-data[,1:60,]
fit<-kmeans(x, 2)
y<-data[,61]
1-sum(fit$cluster*2-3==y)/length(y)