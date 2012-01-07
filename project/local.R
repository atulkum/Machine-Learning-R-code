### How to use locfit to do local regression
### Also see:  http://cm.bell-labs.com/cm/ms/departments/sia/project/locfit/

postscript("plot.ps",horizontal=F)
par(mfrow=c(2,2))

library(locfit)


n = 1000
x = seq(0,1,length=n)
r = sqrt(x*(1-x))*sin(2.1*pi/(x+.05))
y = r + rnorm(n,0,.2)
plot(x,r,type="l",lwd=3,ylim=c(-1,1))
plot(x,y,ylim=c(-1,1));lines(x,r,lwd=3)

h      = seq(.005,.04,length=20)
alpha  = cbind(rep(0,length(h)),h)
out    = gcvplot(y ~ x,alpha=alpha,deg=1,maxk=500,ev="data")
plot(h,out$values,type="l",lwd=3,ylab="GCV")
h      = h[out$values == min(out$values)]
cat("h = ",h,"\n")

out = locfit(y ~ x,alpha = c(0,h),deg=1,maxk=500)

plot(x,fitted(out),type="l",lwd=3,ylim=c(-1,1),ylab="")
points(x,y)
dev.off()




