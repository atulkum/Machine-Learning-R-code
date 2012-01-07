setwd("C:/mine/stat202")
x<-read.csv("cluster.csv",header=F)

plot(x,pch=19,xlab=expression(x[1]),
              ylab=expression(x[2]))
fit<-kmeans(x, 2)

points(fit$centers,pch=19,col="blue",cex=2)

points(x,col=fit$cluster,pch=19)
