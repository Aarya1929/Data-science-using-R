#case study 1 - Pollutant data----
f<-read.csv("pollutant_csv.csv")
m<-mean(f$Temp[f$Month==6])
cat("\nMean of Temperature for month=6:",m)
n<-nrow(f)
cat("\nNo. of observations:",n)
print(tail(f,2))
Oz<-f$Ozone[47]
cat("\nValue of Ozone in 47th row:",Oz)
ms<-sum(is.na(f$Ozone))
cat("\nNo. of missing values in Ozone:",ms)
mo<-mean(f$Ozone,na.rm = T)
cat("\nMean of Ozone excluding NA:",mo)
msr<-mean(f$Solar.R[f$Ozone>31 & f$Temp>90],na.rm = T)
cat("\nMean of Solar.R:",msr)
mx<-max(f$Ozone[f$Month==5],na.rm = T)
cat("\nMax value of Ozone in May:",mx)

#case study 2 - Hair eye color data----
f1<-read.csv("dataset_LAB_1_hair_eye_color_csv.csv")
br<-sum(f1$Eye.Color=="Brown")
cat("\nPeople with Brown eye color:",br)
bl<-sum(f1$Hair.Color=="Blonde")
cat("\nNo.of people with BLonde hair color:",bl)
pe<-sum(f1$Hair.Color[f1$Eye.Color=="Black"]=="Brown")
cat("\nNo. of brown haired people with Black eyes:",pe)
ge<-sum(f1$Eye.Color=="Green")*100/nrow(f1)
cat("\nPercentage of people with green eyes:",ge)
red<-sum(f1$Hair.Color[f1$Eye.Color=="Blue"]=="Red")*100/nrow(f1)
cat("\nPercentage of people with red hair and blue eyes:",red)

#case study 3 - Germination data set----
f2<-read.csv("dataset_LAB_1_germination_csv.csv")
av<-mean(f2$germinated[f2$Box=="Uncovered" & f2$water_amt==4])
cat("\nAverage number of seeds germinated for the uncovered boxes with level of watering equal to 4:",av)
med<-median(f2$germinated[f2$Box=="Covered"])
cat("\nMedian value for the data covered boxes:",med)

#Box plot----
library(ggplot2)
p<-ggplot(iris,aes(Sepal.Length,Species,fill=Species))+geom_boxplot(outlier.color = "Red",outlier.size = 4,outlier.shape = 4)+theme(legend.position = "none")+labs(title="BOXPLOT",x="sepallength",y="species")+coord_flip()
print(p)

#Scatter plot----
library(dslabs)
p1<-ggplot(murders,aes(population/10^6,total,label=abb))+geom_point(aes(color=region))+scale_x_log10()+scale_y_log10()+geom_text(size=3,nudge_x = 0.075)+labs(title = "SCATTERPLOT",x="POPULATION",y="TOTAL")
print(p1)