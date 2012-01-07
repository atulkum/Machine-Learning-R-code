x<-c(1,2,3,5,6,7,8) 

center1<-1
center2<-2

for (k in 2:10){
  cluster1<-x[abs(x-center1[k-1])<=abs(x-center2[k-1])]
  cluster2<-x[abs(x-center1[k-1])>abs(x-center2[k-1])]
  center1[k]<-mean(cluster1)
  center2[k]<-mean(cluster2)
}
