f<-read.csv("Toy_sales_csv.csv")
#SLR 
l1<-lm(Unitsales~Price,f)
s1<-summary(l1)
print(s1)
library(ggplot2)
p<-ggplot(f,aes(Price,Unitsales))+geom_point()+geom_smooth(method = "lm",formula = y~x,color="red",se=F)
print(p)
pred1<-predict(l1)
print(pred1)
err<-f$Unitsales-pred1
print(err)

#MLR
l2<-lm(Unitsales~Price+Adexp+Promexp,f)#all variables to be considered
s2<-summary(l2)
print(s2)

df1<-data.frame(Price=c(9.1,8.1),Adexp=c(52,50),Promexp=c(61,60))
pred2<-predict(l2,df1)
result<-cbind(df1,pred2)
print(result)