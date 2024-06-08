setwd("D:/R")
f<-read.csv("travelled abroad_csv.csv")
p<-sum(f$Travelledabroad=="Y")/nrow(f)
cat("\nProbability of success:",p)
per<-p*100
cat("\nPercentage of people travelled abroad:",per)

#n=10 case
d1<-dbinom(0:10,10,p)
cat("\nProbability for k=0:10 with B.D.:",d1)

plot(0:10,d1,type="l")

#n=100 case
m<-100*p
cat("\nMean is:",m)

s<-sqrt(100*p*(1-p))
cat("\nStandard Deviation:",s)

p1<-pnorm(59,m,s,lower.tail = F)
cat("\nProbability for n=100 with N.D. is:",p1)

d2<-sum(dbinom(59:100,100,p))
cat("\nProbability for n=100 with B.D. is:",d2)

d3<-dbinom(0:100,100,p)
plot(0:100,d3,type = "l")
