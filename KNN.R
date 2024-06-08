f<-read.csv("knn1_csv.csv")
eucld<-sqrt((f$x-3)**2+(f$y-2)**2)
data<-cbind(f,eucld)
sort_data<-data[order(data$eucld),]

#NN algorithm
cl<-sort_data[1,4]
cat("Class of P(3,2) for NN: ",cl)

#KNN algorithm for k=5
df1<-sort_data[1:5,]
s1<-sum(df1$class==1)
s2<-sum(df1$class==2)
s3<-sum(df1$class==3)
if(s1>s2 & s1>s3)
{
  cat("\nClass of P(3,2) for KNN with K=5: 1");
}
if(s2>s1 & s2>s3)
{
  cat("\nClass of P(3,2) for KNN with K=5: 2");
}
if(s3>s1 & s3>s2)
{
  cat("\nClass of P(3,2) for KNN with K=5: 3");
}

#RNN with radius=1.45
df2<-sort_data[sort_data$eucld<1.45,]
s11<-sum(df2$class==1)
s22<-sum(df2$class==2)
s33<-sum(df2$class==3)
if(s11>s22 & s11>s33)
{
  cat("\nClass of P(3,2) for RNN with radius=1.45: 1");
}
if(s22>s11 & s22>s33)
{
  cat("\nClass of P(3,2) for RNN with radius=1.45: 2");
}
if(s33>s11 & s33>s22)
{
  cat("\nClass of P(3,2) for RNN with radius=1.45: 3");
}

#KNN algorithm for k=7
df3<-sort_data[1:7,]
s111<-sum(df3$class==1)
s222<-sum(df3$class==2)
s333<-sum(df3$class==3)
if(s111>s222 & s111>s333)
{
  cat("\nClass of P(3,2) for KNN with K=7: 1");
}
if(s222>s111 & s222>s333)
{
  cat("\nClass of P(3,2) for KNN with K=7: 2");
}
if(s333>s111 & s333>s222)
{
  cat("\nClass of P(3,2) for KNN with K=7: 3");
}
