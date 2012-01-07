data<-read.csv("sonar_train.csv",header=FALSE)

x<-data[,1:2,]

plot(x,pch=19,xlab=expression(x[1]),
              ylab=expression(x[2]))
fit<-kmeans(x, 2)

points(fit$centers,pch=19,col="blue",cex=2)

points(x,col=fit$cluster,pch=19)

plot(x,pch=19,xlab=expression(x[1]),
              ylab=expression(x[2]))

y<-data[,61]

points(x,col=2+2*y,pch=19)

sum(fit$cluster*2-3==y)/length(y)
