wbc<-read.csv("wbc_csv.csv")
wbc$diagnosis<-as.factor(wbc$diagnosis)
set.seed(123)
wbc<-wbc[sample(nrow(wbc)),]

#Normalization - scaling data b/w 0 to 1
wbc_mod<-wbc[,3:32]
n2<-function(b){
  (b-min(b))/(max(b)-min(b))
}
wbc1<-as.data.frame(lapply(wbc_mod, n2))

#splitting data into training and testing
train<-wbc1[1:469,]
test<-wbc1[470:569,]
train_label<-wbc[1:469,2]
test_label<-wbc[470:569,2]

#knn
library(class)
p<-knn(train,test,train_label,k=7)
t<-table(Actual=test_label,predicted=p)
print(t)
acc<-sum(diag(t))/sum(t)
cat("\nAccuracy: ",acc)
sen<-t[2,2]/sum(t[2,])
cat("\nSensitivity: ",sen)
spe<-t[1,1]/sum(t[1,])
cat("\nSpecificity: ",spe)
pre<-t[2,2]/sum(t[,2])
cat("\nPrecision: ",pre)
