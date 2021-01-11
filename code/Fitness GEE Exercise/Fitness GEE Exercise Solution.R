fitness.data<- read.csv(file="Fitness GEE Exercise/FitnessData.csv", header=TRUE, sep=",")

#creating longform dataset
library(reshape2)
data1<- melt(fitness.data[,c("id","gender","age","oxygen1","oxygen2","oxygen3")],
             id.vars=c("id","gender","age"),variable.name="oxygen.cond",value.name="oxygen")
data2<- melt(fitness.data[,c("runtime1","runtime2","runtime3")],variable.name="runtime.cond",value.name="runtime")
data3<- melt(fitness.data[,c("pulse1","pulse2","pulse3")],variable.name="pulse.cond",value.name="pulse")
longform.data<- cbind(data1,data2,data3)

#sorting long-form data set by id
longform.data<- longform.data[order(longform.data$id),]

#creating variable for condition
condition<- ifelse(longform.data$pulse.cond=="pulse1",1,ifelse(longform.data$pulse.cond=="pulse2",2,3))


library(geepack)
library(MuMIn)
#fitting GEE model with unstructured working correlation matrix
summary(un.fitted.model<- geeglm(pulse ~ gender + age + oxygen + runtime 
                                 + condition,data=longform.data,id=id, family = gaussian(link="identity"), 
                                 corstr = "unstructured"))
QIC(un.fitted.model)

#fitting GEE model with autoregressive working correlation matrix
summary(ar.fitted.model<- geeglm(pulse ~ gender + age + oxygen + runtime 
                                 + condition,data=longform.data,id=id, family = gaussian(link="identity"), 
                                 corstr = "ar1"))
QIC(ar.fitted.model)

#fitting GEE model with compound symmetric working correlation matrix
summary(cs.fitted.model<- geeglm(pulse ~ gender + age + oxygen + runtime 
                                 + condition,data=longform.data,id=id, family = gaussian(link="identity"),
                                 corstr = "exchangeable"))
QIC(cs.fitted.model)

#fitting GEE model with independent working correlation matrix
summary(ind.fitted.model<- geeglm(pulse ~ gender + age + oxygen + runtime 
                                  + condition,data=longform.data,id=id, family = gaussian(link="identity"),
                                  corstr = "independence"))
QIC(ind.fitted.model)

#using best-fitted model for prediction
print(predict(ind.fitted.model, data.frame(gender="F", age=36, oxygen=40.2, 
                                           runtime=10.3, condition=1)))
