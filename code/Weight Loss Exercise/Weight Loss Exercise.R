weightloss.data <- read.csv(file="Weight Loss Exercise/WeightLossData.csv", header=TRUE, sep=",")

#creating longform dataset
library(reshape2)
data1 <- melt(weightloss.data[,c("id","group","gender","aexercise","bexercise","cexercise")],
              id.vars=c("id","group","gender"),variable.name="exercise.visit",value.name="exercise")
data2 <- melt(weightloss.data[,c("aBMI","bBMI","cBMI")],variable.name="BMI.visit",value.name="BMI")
longform.data<- cbind(data1,data2)

#sorting long-form data set by id
longform.data<- longform.data[order(longform.data$id),]

#creating variable for time
month <- ifelse(longform.data$BMI.visit=="aBMI",0,ifelse(longform.data$BMI.visit=="bBMI",1,3))

#(a) Verify normality of the response variable BMI by plotting the histogram and carrying out the normality test.
#(b) Fit the random slope and intercept model. Present the fitted model and specify all estimated parameters. Discuss significance of the parameters at the 5% significance level.
#(c) Give interpretation of the estimated significant beta coefficients. Is the intervention efficient?
#(d) Compute the predicted BMI at 3 months for an intervention group male participant, if he exercises for 1 hour every day.
#(e) Fit the GEE models with unstructured, autoregressive, compound symmetric, and independent working correlation matrices of the response variable BMI.
#(f) Choose the best-fitted model with respect to the QIC criterion.
#(g) For the best-fitted model, write down the fitted model. Estimate all parameters. Discuss what predictors are significant at the 5% level.
#(h) Interpret the estimated significant regression coefficients.
#(i) Compute the predicted BMI at 3 months for an intervention group male participant, if he exercises for 1 hour every day.
