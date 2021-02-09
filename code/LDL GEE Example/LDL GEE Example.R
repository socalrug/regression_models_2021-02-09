cholesterol.data<- read.csv(file="LDL GEE Example/LDLData.csv", header=TRUE, sep=",")

#creating long-form data set
library(reshape2)
longform.data<- melt(cholesterol.data, id.vars=c("id", "gender", "age"), 
                     variable.name = "LDLmonth", value.name="LDL")

#sorting long-form data set by id
longform.data<- longform.data[order(longform.data$id),]

#creating variable for time
month<- ifelse(longform.data$LDLmonth=="LDL0", 0, ifelse(longform.data$LDLmonth
                                                         =="LDL6", 6, ifelse(longform.data$LDLmonth=="LDL9",9,24)))

library(geepack)
#fitting GEE model with unstructured working correlation matrix
summary(un.fitted.model<- geeglm(LDL ~ gender + age + month, 
                                 data=longform.data, id=id, family=gaussian(link="identity"), 
                                 corstr="unstructured"))

#fitting GEE model with autoregressive working correlation matrix
summary(ar.fitted.model<- geeglm(LDL ~ gender + age + month, 
                                 data=longform.data, id=id, family=gaussian(link="identity"), 
                                 corstr="ar1"))

library(MuMIn)
QIC(ar.fitted.model)

#fitting GEE model with compound symmetric (exchangeable) 
#working correlation matrix
summary(cs.fitted.model<- geeglm(LDL ~ gender + age + month, 
                                 data=longform.data, id=id, family=gaussian(link="identity"), 
                                 corstr="exchangeable"))
QIC(cs.fitted.model)

#fitting GEE model with independent working correlation matrix
summary(ind.fitted.model<- geeglm(LDL ~ gender + age + month, 
                                  data=longform.data, id=id, family=gaussian(link="identity"), 
                                  corstr="independence"))
QIC(ind.fitted.model)

#using autoregressive fitted model for prediction
print(predict(ar.fitted.model, data.frame(gender="F", age=48, month=3)))
