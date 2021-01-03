fitness.data<- read.csv(file="C:/./FitnessData.csv", header=TRUE, sep=",")

#creating longform dataset
library(reshape2)
data1<- melt(fitness.data[,c("id","gender","age","oxygen1","oxygen2","oxygen3")],
id.vars=c("id","gender","age"),variable.name="oxygen.cond",value.name="oxygen")
data2<- melt(fitness.data[,c("runtime1","runtime2","runtime3")],variable.name="runtime.cond",value.name="runtime")
data3<- melt(fitness.data[,c("pulse1","pulse2","pulse3")],variable.name="pulse.cond",value.name="pulse")
longform.data<- cbind(data1,data2,data3)

condition<- ifelse(longform.data$pulse.cond=="pulse1",1,ifelse(longform.data$pulse.cond=="pulse2",2,3))

#(a) Check that pulse has a normal distribution. 
#Construct a histogram and conduct normality tests.

#(b) Run a random slope and intercept regression model for pulse. 
#Variables for analysis are: id gender age oxygen runtime pulse condition
#Write down the fitted model.

#(c) Discuss significance of predictors at the 5% level. 
#Interpret estimated significant regression coefficients. 

#(d) Predict an average heart rate for a 36-year-old woman who is 
#running on a treadmill, if her oxygen intake is 40.2 units, and her 
#run time is 10.3 minutes per mile.




