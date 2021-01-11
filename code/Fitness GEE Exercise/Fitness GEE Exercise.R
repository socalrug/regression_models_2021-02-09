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

#(a) Run GEE models with unstructured, autoregressive, compound symmetric, 
#and independent working correlation matrices. Output QICs.

#(b) Find the optimal model according to the QIC criterion.

#(c) For the optimal model, write down the fitted model, estimating 
#all parameters.

#(d) Discuss significance of predictors and interpret significant estimated 
#regression coefficients. 

#(e) Predict an average heart rate for a 36-year-old woman who 
#is running on #a treadmill, if her oxygen intake is 40.2 units, and her 
#run time is 10.3 minutes per mile.
