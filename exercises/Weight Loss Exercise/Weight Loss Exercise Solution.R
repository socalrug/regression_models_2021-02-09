weightloss.data<- read.csv(file="C:/./WeightLossData.csv", header=TRUE, sep=",")

#creating longform dataset
library(reshape2)
data1<- melt(weightloss.data[,c("id","group","gender","aexercise","bexercise","cexercise")],
id.vars=c("id","group","gender"),variable.name="exercise.visit",value.name="exercise")
data2<- melt(weightloss.data[,c("aBMI","bBMI","cBMI")],variable.name="BMI.visit",value.name="BMI")
longform.data<- cbind(data1,data2)

#sorting long-form data set by id
longform.data<- longform.data[order(longform.data$id),]

#creating variable for time
month<- ifelse(longform.data$BMI.visit=="aBMI",0,ifelse(longform.data$BMI.visit=="bBMI",1,3))

#checking normality of response
library(rcompanion)
plotNormalHistogram(longform.data$BMI)
shapiro.test(longform.data$BMI)

#fitting random slope and intercept model
library(nlme)
summary(fitted.model<- lme(BMI ~ group + gender + exercise
+ month, random = ~ 1 + month | id, data=longform.data))

#using fitted model for prediction
print(predict(fitted.model, data.frame(gender=1, group=1,
exercise=60, month=3),level=0))

#fitting GEE models
library(geepack)
library(MuMIn)
#fitting GEE model with unstructured working correlation matrix
summary(un.fitted.model<- geeglm(BMI ~ group + gender + exercise
+ month, data=longform.data, id=id, family=gaussian(link="identity"), 
corstr = "unstructured"))
QIC(un.fitted.model)

#fitting GEE model with autoregressive working correlation matrix
summary(ar.fitted.model<- geeglm(BMI ~ group + gender + exercise
+ month, data=longform.data, id=id, family=gaussian(link="identity"), 
corstr = "ar1"))
QIC(ar.fitted.model)

#fitting GEE model with compound symmetric working correlation matrix
summary(cs.fitted.model<- geeglm(BMI ~ group + gender + exercise
+ month, data=longform.data, id=id, family=gaussian(link="identity"), 
corstr = "exchangeable"))
QIC(cs.fitted.model)

#fitting GEE model with independent working correlation matrix
summary(ind.fitted.model<- geeglm(BMI ~ group + gender + exercise
+ month, data=longform.data, id=id, family=gaussian(link="identity"), 
corstr = "independence"))
QIC(ind.fitted.model)

#using best-fitted GEE model for prediction
print(predict(un.fitted.model, data.frame(group="Int", gender="F",
exercise=60, month=3)))
