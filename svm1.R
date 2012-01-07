x<-matrix(c(0,.1,.8,.9,.4,.5,.3,.7,.1,.4,.7,.3,.5,.2,.8,.6,.8,0,.8,.3), ncol=2,byrow=T)

y<-as.factor(c(rep(-1,5),rep(1,5)))

plot(x,pch=19,xlim=c(0,1),ylim=c(0,1), col=2*as.numeric(y),cex=2, xlab=expression(x[1]),ylab=expression(x[2]))

fit<-svm (x,y,kernel="linear",cost=100000)

big_x<-matrix(runif(200000),ncol=2,byrow=T)

points(big_x,col=rgb(.5,.5, .2+.6*as.numeric(predict(fit,big_x)==1)),pch=19)
points(x,pch=19,col=2*as.numeric(y),cex=2)

abline(c(-0.04,1))