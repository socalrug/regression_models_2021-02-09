fitness.data<- read.csv(file="C:/./FitnessData.csv", header=TRUE, sep=",")

#creating longform dataset
library(reshape2)
data1<- melt(fitness.data[,c("id","gender","age","oxygen1","oxygen2","oxygen3")],
id.vars=c("id","gender","age"),variable.name="oxygen.cond",value.name="oxygen")
data2<- melt(fitness.data[,c("runtime1","runtime2","runtime3")],variable.name="runtime.cond",value.name="runtime")
data3<- melt(fitness.data[,c("pulse1","pulse2","pulse3")],variable.name="pulse.cond",value.name="pulse")
longform.data<- cbind(data1,data2,data3)
condition<- ifelse(longform.data$pulse.cond=="pulse1",1,ifelse(longform.data$pulse.cond=="pulse2",2,3))

#plotting histogram and checking normality
library(rcompanion)
plotNormalHistogram(longform.data$pulse)
shapiro.test(longform.data$pulse)

#fitting random slope and intercept model
library(nlme)
summary(fitted.model<- lme(pulse ~ gender + age + oxygen + runtime
+ condition, random = ~ 1 + condition | id, control=lmeControl(opt="optim"), 
data=longform.data))

#using fitted model for prediction
print(predict(fitted.model, data.frame(gender=0, age=36, oxygen=40.2, 
runtime=10.3, condition=1),level=0))




