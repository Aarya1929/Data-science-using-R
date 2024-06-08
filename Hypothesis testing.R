#Question 1
f1<-read.csv("Hypothesis_csv1.csv")
m1<-mean(f1$Life_Hrs)
cat("Observed value is:",m1)
sd1<-sd(f1$Life_Hrs)
cat("\nStandard Deviation is:",sd1)
se1<-sd1/sqrt(50)
cat("\nStandard Error is:",se1)
p1<-pnorm(m1,10000,se1)
cat("\npvalue for 1st case is:",p1)
#When alpha=0.05
if(p1<0.05)
{
  cat("\nManufacturer claim can be rejected for 0.05 significance level")
}else
{
  cat("\nManufacturer claim can not be rejected for 0.05 significance level")
}
#When alpha=0.01
if(p1<0.01)
{
  cat("\nManufacturer claim can be rejected for 0.01 significance level")
}else
{
  cat("\nManufacturer claim can not be rejected for 0.01 significance level")
}

#Question 2
se2<-17/sqrt(35)
cat("\n\nStandard Error is:",se2)
p2<-2*pnorm(134,130,se2,lower.tail = F)
cat("\npvalue for 1st case is:",p2)
#When alpha=0.05
if(p2<0.05)
{
  cat("\nClaim can be rejected for 0.05 significance level, it is not accurate")
}else
{
  cat("\nClaim can not be rejected for 0.05 significance level, it is accurate")
}
#When alpha=0.01
if(p2<0.01)
{
  cat("\nClaim can be rejected for 0.01 significance level, it is not accurate")
}else
{
  cat("\nClaim can not be rejected for 0.01 significance level, it is accurate")
}

