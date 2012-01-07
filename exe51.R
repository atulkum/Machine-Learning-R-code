setwd("C:\\mine\\stat202\\hw6")

data<-read.csv("spring2008exams.csv") 

exam2mean<-mean(data[,3],na.rm=TRUE) 

exam2sd<-sd(data[,3],na.rm=TRUE)

z<-(data[,3]-exam2mean)/exam2sd

sort(z)


data<-read.csv("spring2008exams.csv") 

q1<-quantile(data[,3],.25,na.rm=TRUE)
q3<-quantile(data[,3],.75,na.rm=TRUE)
iqr<-q3-q1

data[(data[,3]>q3+1.5*iqr),3]
data[(data[,3]<q1-1.5*iqr),3]

boxplot(data[,2],data[,3],col="blue",
main="Exam Scores",
names=c("Exam 1","Exam 2"),ylab="Exam Score")


data<-read.csv("spring2008exams.csv")
model<-lm(data[,3]~data[,2])
plot(data[,2],data[,3],pch=19,xlab="Exam 1", ylab="Exam2",xlim=c(50,110),ylim=c(50,110))
abline(model)
sort(model$residuals)
