cholesterol.data<- read.csv(file="C:/./LDLData.csv", header=TRUE, sep=",")

#creating long-form data set
library(reshape2)
longform.data<- melt(cholesterol.data, id.vars=c("id", "gender", "age"), 
variable.name = "LDLmonth", value.name="LDL")

#creating variable for time
month<- ifelse(longform.data$LDLmonth=="LDL0", 0, ifelse(longform.data$LDLmonth
=="LDL6", 6, ifelse(longform.data$LDLmonth=="LDL9",9,24)))

#plotting histogram with fitted normal density
library(rcompanion)
plotNormalHistogram(longform.data$LDL)

#testing for normality of distribution 
shapiro.test(longform.data$LDL)

#fitting random slope and intercept model
library(nlme)

summary(fitted.model<- lme(LDL ~ gender+age+month, 
random =~ 1+month|id, control=lmeControl(opt="optim"), 
data=longform.data))

#ploting variance of response
variance<- function(t) {
  590.015-32.816*t+0.785*t^2
}
t<- 1:30
plot(t,variance(t), xlab="month", ylab="variance")

#plotting individual profiles
tr.data<- t(cholesterol.data)[-(1:3),]

matplot(tr.data, type="b", pch=16, lty=1, 
col=1:27, axes=FALSE, ylab="LDL", xlab="month")

xticks=c("0", "", "6", "", "9", "", "24")
axis(1,at=seq(1,4,0.5),labels=xticks)
axis(2)

#using fitted model for prediction
predict(fitted.model, data.frame(gender=0, age=48, 
month=3), level=0)


