x<-c(1,2,2.5,3,3.5,4,4.5,5,7,8,8.5,9,9.5,10)

center1<-1
center2<-2

for (k in 2:10){
  cluster1<-x[abs(x-center1[k-1])<=abs(x-center2[k-1])]
  cluster2<-x[abs(x-center1[k-1])>abs(x-center2[k-1])]
  center1[k]<-mean(cluster1)
  center2[k]<-mean(cluster2)
}
center1
center2
cluster1
cluster2

x<-c(1,2,2.5,3,3.5,4,4.5,5,7,8,8.5,9,9.5,10)
fit<-kmeans(x,2)