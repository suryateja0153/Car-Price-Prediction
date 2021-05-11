rm(list=ls())
#install.packages("rio",dep=TRUE)
#install.packages("moments",dep=TRUE)
library(rio)
library(moments)
library(car)
library(lmtest)
library(ggplot2)
library(dplyr)

# Get Data.
price=import("CarPrice Data.xlsx",sheet="Sheet1")
colnames(price)=tolower(make.names(colnames(price)))
attach(price)
names(price)
str(price)
price$doornumber_x3 = as.factor(price$doornumber_x3)
str(price)
attach(price)

# ------------------------------------------------------------------------------
# Q1
# 1.1 (y,X1)
# Basic scatterplot of the data.
plot(horsepower_x1,price_y,pch=19,
     main="Car Price Data",xlab="Horsepower",
     ylab="Price")

# Conducting a Simple regression (lm="Linear Model") on the data.
price1.1=lm(price_y~horsepower_x1)
summary(price1.1)
confint(price1.1)
par(mfrow=c(2,2))
plot(price1.1,pch=19)
par(mfrow=c(1,1))
ggplot(price,aes(horsepower_x1,price_y))+
        geom_point()+
        geom_smooth(method="lm",color="red")

# Adding the regression line to the plot.  
# Slope and intercept are pulled from the regression output.
plot(horsepower_x1,price_y,pch=19,
     main="Car Price Data",xlab="Horsepower",
     ylab="Price")
abline(price1.1,col="red",lwd=3)

# LINE Assumptions of Regression
# Linearity
plot(price$price_y,price1.1$fitted.values,pch=19,
     main="Actual v. Fitted Values, Price vs Horsepower")
abline(0,1,col="red",lwd=3)

# Independence of the observations.
dwtest(lm(price_y~horsepower_x1))
chisq.test(horsepower_x1,price_y, correct = FALSE)

# Normality
# A normal probability plot of the residuals.
qqnorm(price1.1$residuals,pch=19)
qqline(price1.1$residuals,col="red",lwd=3)

# Equality of Variances
# Plot of residuals.  These are not standardized residuals.
plot(price$price_y,price1.1$residuals,pch=19,
     main="Residual Plot")
abline(0,0,col="red",lwd=3)

# Plotting of standardized residuals.
plot(price$price_y,rstandard(price1.1),
     main="Standardized Residual Plot",
     pch=19)
abline(0,0,col="red",lwd=3)

# 1.2 (y,X2)
# Basic scatterplot of the data.
plot(peakrpm_x2,price_y,pch=19,
     main="Car Price Data",xlab="Peak RPM",
     ylab="Price")

# Conducting a Simple regression (lm="Linear Model") on the data.
price1.2=lm(price_y~peakrpm_x2)
summary(price1.2)
confint(price1.2)
par(mfrow=c(2,2))
plot(price1.2,pch=19)
par(mfrow=c(1,1))
ggplot(price,aes(peakrpm_x2,price_y))+
        geom_point()+
        geom_smooth(method="lm",color="red")

# Adding the regression line to the plot.  
# Slope and intercept are pulled from the regression output.
plot(peakrpm_x2,price_y,pch=19,
     main="Car Price Data",xlab="Peak RPM",
     ylab="Price")
abline(price1.2,col="red",lwd=3)

# LINE Assumptions of Regression
# Linearity
plot(price$price_y,price1.2$fitted.values,pch=19,
     main="Actual v. Fitted Values, Price vs Peak RPM")
abline(0,1,col="red",lwd=3)

# Independence of the observations.
dwtest(lm(price_y~peakrpm_x2))
chisq.test(peakrpm_x2,price_y, correct = FALSE)

# Normality
# A normal probability plot of the residuals.
qqnorm(price1.2$residuals,pch=19)
qqline(price1.2$residuals,col="red",lwd=3)

# Equality of Variances
# Plot of residuals.  These are not standardized residuals.
plot(price$price_y,price1.2$residuals,pch=19,
     main="Residual Plot")
abline(0,0,col="red",lwd=3)

# Plotting of standardized residuals.
plot(price$price_y,rstandard(price1.2),
     main="Standardized Residual Plot",
     pch=19)
abline(0,0,col="red",lwd=3)

# 1.3 (y,X3)
# Basic scatterplot of the data.
plot(doornumber_x3,price_y,pch=19,
     main="Car Price Data",xlab="No. of Doors",
     ylab="Price")

# Conducting a Simple regression (lm="Linear Model") on the data.
price1.3=lm(price_y~doornumber_x3)
summary(price1.3)
confint(price1.3)
par(mfrow=c(2,2))
plot(price1.3,pch=19)
par(mfrow=c(1,1))
ggplot(price,aes(doornumber_x3,price_y))+
        geom_point()+
        geom_smooth(method="lm",color="red")

# Adding the regression line to the plot.  
# Slope and intercept are pulled from the regression output.
plot(doornumber_x3,price_y,pch=19,
     main="Car Price Data",xlab="No. of Doors",
     ylab="Price")
abline(price1.3,col="red",lwd=3)

# LINE Assumptions of Regression
# Linearity
plot(price$price_y,price1.3$fitted.values,pch=19,
     main="Actual v. Fitted Values, Price vs No. of Doors")
abline(0,1,col="red",lwd=3)

# Independence of the observations.
dwtest(lm(price_y~doornumber_x3))
chisq.test(doornumber_x3,price_y, correct = FALSE)

# Normality
# A normal probability plot of the residuals.
qqnorm(price1.3$residuals,pch=19)
qqline(price1.3$residuals,col="red",lwd=3)

# Equality of Variances
# Plot of residuals.  These are not standardized residuals.
plot(price$price_y,price1.3$residuals,pch=19,
     main="Residual Plot")
abline(0,0,col="red",lwd=3)

# Plotting of standardized residuals.
plot(price$price_y,rstandard(price1.3),
     main="Standardized Residual Plot",
     pch=19)
abline(0,0,col="red",lwd=3)

# ------------------------------------------------------------------------------
# Q2
# 2.1 (y,X1,X2)
# Separate regressions on Horsepower and Peak RPM.
hp=lm(price_y~horsepower_x1)
summary(hp)
pr=lm(price_y~peakrpm_x2)
summary(pr)

# Plotting Horsepower original data.
plot(horsepower_x1,price_y,
     pch=19,main="Car Price Comparative Plot")

# Overlaying points for Peak RPM.
points(peakrpm_x2,price_y,col="red",pch=17)

# Adding separate regression lines for Horsepower and Peak RPM.
abline(hp,lwd=3)
abline(pr,col="red",lwd=3)

# Graph with residuals for Horsepower and Peak RPM.
plot(horsepower_x1,hp$residuals,pch=19,
     main="Comparative Residual Plot")
points(peakrpm_x2,pr$residuals,pch=17,
       col="red")
abline(0,0,col="blue",lwd=3)

# Normal probability plot of Horsepower and Peak RPM residuals.
par(mfrow=c(1,2))
qqnorm(hp$residuals,
       main="Horsepower Normal Plot",pch=19)
qqline(hp$residuals,lwd=3,col="red")
qqnorm(pr$residuals,
       main="Peak RPM Normal Plot",pch=19)
qqline(pr$residuals,lwd=3,col="red")
par(mfrow=c(1,1))

# Multiple regression for Horsepower and Peak RPM.
price2.1 = lm(price_y~horsepower_x1+peakrpm_x2,data = price)
summary(price2.1)
confint(price2.1)
par(mfrow=c(2,2))
plot(price1.1,pch=19)
par(mfrow=c(1,1))
ggplot(price)+
        geom_point(aes(horsepower_x1,price_y))+
        geom_smooth(aes(horsepower_x1,price_y),method="lm",color="red",se=FALSE)+
        geom_point(aes(peakrpm_x2,price_y))+
        geom_smooth(aes(peakrpm_x2,price_y),method="lm",color="blue",se=FALSE)

# LINE Assumptions of Regression.
# Linearity
plot(price$price_y,price2.1$fitted.values,pch=19,
     main="Actual v. Fitted Values, Price vs Horsepower and Peak RPM")
abline(0,1,col="red",lwd=3)

# Independence of the observations.
dwtest(price2.1)

# Normality
# A normal probability plot of the residuals.
qqnorm(price2.1$residuals,pch=19)
qqline(price2.1$residuals,col="red",lwd=3)

# Equality of Variances
# Plot of residuals.  These are not standardized residuals.
plot(price$price_y,price2.1$residuals,pch=19,
     main="Residual Plot")
abline(0,0,col="red",lwd=3)

# Plotting of standardized residuals.
plot(price$price_y,rstandard(price2.1),
     main="Standardized Residual Plot",
     pch=19)
abline(0,0,col="red",lwd=3)
dwtest(price2.1)

# 2.2 (y,X1,X3)
# Separate regressions on Horsepower and No. of Doors.
nd=lm(price_y~doornumber_x3)
summary(nd)

# Plotting Horsepower original data.
plot(horsepower_x1,price_y,
     pch=19,main="Car Price Comparative Plot")

# Overlaying points for No. of Doors.
points(doornumber_x3,price_y,col="red",pch=17)

# Adding separate regression lines for Horsepower and No. of Doors.
abline(hp,lwd=3)
abline(nd,col="red",lwd=3)

# Graph with residuals for Horsepower and No. of Doors.
plot(horsepower_x1,hp$residuals,pch=19,
     main="Comparative Residual Plot")
points(doornumber_x3,nd$residuals,pch=17,
       col="red")
abline(0,0,col="blue",lwd=3)

# Normal probability plot of Horsepower and No. of Doors residuals.
par(mfrow=c(1,2))
qqnorm(hp$residuals,
       main="Horsepower Normal Plot",pch=19)
qqline(hp$residuals,lwd=3,col="red")
qqnorm(nd$residuals,
       main="No. of Doors Normal Plot",pch=19)
qqline(nd$residuals,lwd=3,col="red")
par(mfrow=c(1,1))

# Multiple regression for Horsepower and No. of Doors.
price2.2 = lm(price_y~horsepower_x1+doornumber_x3,data = price)
summary(price2.2)
confint(price2.2)
par(mfrow=c(2,2))
plot(price2.2,pch=19)
par(mfrow=c(1,1))
ggplot(price)+
        geom_point(aes(horsepower_x1,price_y))+
        geom_smooth(aes(horsepower_x1,price_y),method="lm",color="red",se=FALSE)+
        geom_point(aes(doornumber_x3,price_y))+
        geom_smooth(aes(doornumber_x3,price_y),method="lm",color="blue",se=FALSE)

# LINE Assumptions of Regression.
# Linearity
plot(price$price_y,price2.2$fitted.values,pch=19,
     main="Actual v. Fitted Values, Price vs Horsepower and No. of Doors")
abline(0,1,col="red",lwd=3)

# Independence of the observations.
dwtest(price2.2)

# Normality
# A normal probability plot of the residuals.
qqnorm(price2.2$residuals,pch=19)
qqline(price2.2$residuals,col="red",lwd=3)

# Equality of Variances
# Plot of residuals.  These are not standardized residuals.
plot(price$price_y,price2.2$residuals,pch=19,
     main="Residual Plot")
abline(0,0,col="red",lwd=3)

# Plotting of standardized residuals.
plot(price$price_y,rstandard(price2.2),
     main="Standardized Residual Plot",
     pch=19)
abline(0,0,col="red",lwd=3)

# 2.3 (y,X2,X3)
# Plotting Peak RPM original data.
plot(peakrpm_x2,price_y,
     pch=19,main="Car Price Comparative Plot")

# Overlaying points for No. of Doors.
points(doornumber_x3,price_y,col="red",pch=17)

# Adding separate regression lines for Peak RPM and No. of Doors.
abline(pr,lwd=3)
abline(nd,col="red",lwd=3)

# Graph with residuals for Peak RPM and No. of Doors.
plot(peakrpm_x2,pr$residuals,pch=19,
     main="Comparative Residual Plot")
points(doornumber_x3,nd$residuals,pch=17,
       col="red")
abline(0,0,col="blue",lwd=3)

# Normal probability plot of Peak RPM and No. of Doors residuals.
par(mfrow=c(1,2))
qqnorm(pr$residuals,
       main="Peak RPM Normal Plot",pch=19)
qqline(pr$residuals,lwd=3,col="red")
qqnorm(nd$residuals,
       main="No .of Doors Normal Plot",pch=19)
qqline(nd$residuals,lwd=3,col="red")
par(mfrow=c(1,1))

# Multiple regression for Peak RPM and No. of Doors.
price2.3 = lm(price_y~peakrpm_x2+doornumber_x3,data = price)
summary(price2.3)
confint(price2.3)
par(mfrow=c(2,2))
plot(price2.3,pch=19)
par(mfrow=c(1,1))
ggplot(price)+
        geom_point(aes(peakrpm_x2,price_y))+
        geom_smooth(aes(peakrpm_x2,price_y),method="lm",color="red",se=FALSE)+
        geom_point(aes(doornumber_x3,price_y))+
        geom_smooth(aes(doornumber_x3,price_y),method="lm",color="blue",se=FALSE)

# LINE Assumptions of Regression.
# Linearity
plot(price$price_y,price2.3$fitted.values,pch=19,
     main="Actual v. Fitted Values, Price vs Peak RPM and No. of Doors")
abline(0,1,col="red",lwd=3)

# Independence of the observations.
dwtest(price2.3)

# Normality
# A normal probability plot of the residuals.
qqnorm(price2.3$residuals,pch=19)
qqline(price2.3$residuals,col="red",lwd=3)

# Equality of Variances
# Plot of residuals.  These are not standardized residuals.
plot(price$price_y,price2.3$residuals,pch=19,
     main="Residual Plot")
abline(0,0,col="red",lwd=3)

# Plotting of standardized residuals.
plot(price$price_y,rstandard(price2.3),
     main="Standardized Residual Plot",
     pch=19)
abline(0,0,col="red",lwd=3)

# ------------------------------------------------------------------------------
# Q3 (y,X1,X2,X3)
# Plotting Horsepower original data.
plot(horsepower_x1,price_y,
     pch=19,main="Car Price Comparative Plot")

# Overlaying points for Peak RPM.
points(peakrpm_x2,price_y,col="red",pch=17)

# Overlaying points for No .of Doors factor.
points(doornumber_x3,price_y,col="green",pch=17)

# Adding separate regression lines for Horsepower, Peak RPM and No .of Doors.
abline(hp,lwd=3)
abline(pr,col="red",lwd=3)
abline(nd,col="green",lwd=3)

# Graph with residuals for Horsepower, Peak RPM and No .of Doors.
plot(horsepower_x1,hp$residuals,pch=19,
     main="Comparative Residual Plot")
points(peakrpm_x2,pr$residuals,pch=17,
       col="red")
points(doornumber_x3,nd$residuals,pch=17,
       col="green")
abline(0,0,col="blue",lwd=3)

# Normal probability plot of Horsepower, Peak RPM and No .of Doors residuals.
par(mfrow=c(2,2))
qqnorm(hp$residuals,
       main="Horsepower Normal Plot",pch=19)
qqline(hp$residuals,lwd=3,col="red")
qqnorm(pr$residuals,
       main="Peak RPM Normal Plot",pch=19)
qqline(pr$residuals,lwd=3,col="red")
qqnorm(nd$residuals,
       main="No .of Doors Normal Plot",pch=19)
qqline(nd$residuals,lwd=3,col="red")
par(mfrow=c(1,1))

# Multiple regression for Horsepower, Peak RPM and No .of Doors.
price3 = lm(price_y~horsepower_x1+peakrpm_x2+doornumber_x3,data = price)
summary(price3)
confint(price3)
par(mfrow=c(2,2))
plot(price3,pch=19)
par(mfrow=c(1,1))
ggplot(price)+
        geom_point(aes(horsepower_x1,price_y))+
        geom_smooth(aes(horsepower_x1,price_y),method="lm",color="red",se=FALSE)+
        geom_point(aes(peakrpm_x2,price_y))+
        geom_smooth(aes(peakrpm_x2,price_y),method="lm",color="blue",se=FALSE)+
        geom_point(aes(doornumber_x3,price_y))+
        geom_smooth(aes(doornumber_x3,price_y),method="lm",color="green",se=FALSE)

# LINE Assumptions of Regression
# Linearity
plot(price$price_y,price3$fitted.values,pch=19,
     main="Actual v. Fitted Values, Price vs Horsepower, Peak RPM and No. of Doors")
abline(0,1,col="red",lwd=3)

# Independence of the observations.
dwtest(price3)

# Normality
# A normal probability plot of the residuals.
qqnorm(price3$residuals,pch=19)
qqline(price3$residuals,col="red",lwd=3)

# Equality of Variances
# Plot of residuals.  These are not standardized residuals.
plot(price$price_y,price3$residuals,pch=19,
     main="Residual Plot")
abline(0,0,col="red",lwd=3)

# Plotting of standardized residuals.
plot(price$price_y,rstandard(price3),
     main="Standardized Residual Plot",
     pch=19)
abline(0,0,col="red",lwd=3)

# ------------------------------------------------------------------------------
# Q4 (y,X1,X2,X1X2)
# Multiple regression for Horsepower and Peak RPM with interactions
price4=lm(price_y~horsepower_x1+peakrpm_x2+I(horsepower_x1*peakrpm_x2),
           data=price)
summary(price4)
confint(price4)
par(mfrow=c(2,2))
plot(price4,pch=19)
par(mfrow=c(1,1))

# LINE Assumptions of Regression
# Linearity
plot(price$price_y,price4$fitted.values,pch=19,
     main="Actual v. Fitted Values, Price vs Horsepower and Peak RPM")
abline(0,1,col="red",lwd=3)

# Independence of the observations.
dwtest(price4)

# Normality
# A normal probability plot of the residuals.
qqnorm(price4$residuals,pch=19)
qqline(price4$residuals,col="red",lwd=3)

# Equality of Variances
# Plot of residuals.  These are not standardized residuals.
plot(price$price_y,price4$residuals,pch=19,
     main="Residual Plot")
abline(0,0,col="red",lwd=3)

# Plotting of standardized residuals.
plot(price$price_y,rstandard(price4),
     main="Standardized Residual Plot",
     pch=19)
abline(0,0,col="red",lwd=3)

# ------------------------------------------------------------------------------
# Q5
# 5.1 (y,X1,X1^2)
# Simple regression with correcting for the non-linearity for Horsepower.
price5.1=lm(price_y~horsepower_x1+I(horsepower_x1^2),data=price)
summary(price5.1)
par(mfrow=c(2,2))
plot(price5.1,pch=19)
par(mfrow=c(1,1))
plot(horsepower_x1,price_y,pch=19,main="Curved Model Plot")
points(horsepower_x1,price5.1$fitted.values,col="red",pch=19)
plot(price5.1$fitted.values,price5.1$residuals,pch=19,
     main="Equality of Variances")
abline(0,0,col="red",lwd=3)

# LINE Assumptions of Regression
# Linearity
plot(price$price_y,price5.1$fitted.values,pch=19,
     main="Actual v. Fitted Values, Price vs Horsepower")
abline(0,1,col="red",lwd=3)

# Independence of the observations.
dwtest(price5.1)

# Normality
# A normal probability plot of the residuals.
qqnorm(price5.1$residuals,pch=19)
qqline(price5.1$residuals,col="red",lwd=3)

# Equality of Variances
# Plot of residuals.  These are not standardized residuals.
plot(price$price_y,price5.1$residuals,pch=19,
     main="Residual Plot")
abline(0,0,col="red",lwd=3)

# Plotting of standardized residuals.
plot(price$price_y,rstandard(price5.1),
     main="Standardized Residual Plot",
     pch=19)
abline(0,0,col="red",lwd=3)

# 5.2 (y,X2,X2^2)
# Simple regression with correcting for the non-linearity for Peak RPM.
price5.2=lm(price_y~peakrpm_x2+I(peakrpm_x2^2),data=price)
summary(price5.2)
par(mfrow=c(2,2))
plot(price5.2,pch=19)
par(mfrow=c(1,1))
plot(peakrpm_x2,price_y,pch=19,main="Curved Model Plot")
points(peakrpm_x2,price5.2$fitted.values,col="red",pch=19)
plot(price5.2$fitted.values,price5.2$residuals,pch=19,
     main="Equality of Variances")
abline(0,0,col="red",lwd=3)

# LINE Assumptions of Regression
# Linearity
plot(price$price_y,price5.2$fitted.values,pch=19,
     main="Actual v. Fitted Values, Price vs Peak RPM")
abline(0,1,col="red",lwd=3)

# Independence of the observations.
dwtest(price5.2)

# Normality
# A normal probability plot of the residuals.
qqnorm(price5.2$residuals,pch=19)
qqline(price5.2$residuals,col="red",lwd=3)

# Equality of Variances
# Plot of residuals.  These are not standardized residuals.
plot(price$price_y,price5.2$residuals,pch=19,
     main="Residual Plot")
abline(0,0,col="red",lwd=3)

# Plotting of standardized residuals.
plot(price$price_y,rstandard(price5.2),
     main="Standardized Residual Plot",
     pch=19)
abline(0,0,col="red",lwd=3)

# ------------------------------------------------------------------------------
# Best Fit: Q3
# Q3 (y,X1,X2,X3)
# Multiple regression for Horsepower, Peak RPM and No .of Doors.
price3 = lm(price_y~horsepower_x1+peakrpm_x2+doornumber_x3,data = price)
summary(price3)
confint(price3)
par(mfrow=c(2,2))
plot(price3,pch=19)
par(mfrow=c(1,1))
ggplot(price)+
        geom_point(aes(horsepower_x1,price_y))+
        geom_smooth(aes(horsepower_x1,price_y),method="lm",color="red",se=FALSE)+
        geom_point(aes(peakrpm_x2,price_y))+
        geom_smooth(aes(peakrpm_x2,price_y),method="lm",color="blue",se=FALSE)+
        geom_point(aes(doornumber_x3,price_y))+
        geom_smooth(aes(doornumber_x3,price_y),method="lm",color="green",se=FALSE)

# LINE Assumptions of Regression
# Linearity
plot(price$price_y,price3$fitted.values,pch=19,
     main="Actual v. Fitted Values, Price vs Horsepower, Peak RPM and No. of Doors")
abline(0,1,col="red",lwd=3)

# Independence of the observations.
dwtest(price3)
durbinWatsonTest(price3)
acf(price3$residuals)

# Normality
# A normal probability plot of the residuals.
qqnorm(price3$residuals,pch=19)
qqline(price3$residuals,col="red",lwd=3)

# Equality of Variances
# Plot of residuals.  These are not standardized residuals.
plot(price$price_y,price3$residuals,pch=19,
     main="Residual Plot")
abline(0,0,col="red",lwd=3)

# Plotting of standardized residuals.
plot(price$price_y,rstandard(price3),
     main="Standardized Residual Plot",
     pch=19)
abline(0,0,col="red",lwd=3)

# Prediction Interval
saturn=predict(price3,price,interval = "confidence")
jupiter=predict(price3,price,interval = "predict")

saturn=as.data.frame(saturn)
jupiter=as.data.frame(jupiter)
saturn$index=seq(1,nrow(saturn))
jupiter$index=seq(1,nrow(jupiter))
names(saturn)
plot(saturn$index,saturn$fit,type="l",
     lwd=3,col="red",
     main="Comparing Fit Prediction and Confidence Intervals",
     ylim=c(min(jupiter$lwr),max(jupiter$upr)))
points(saturn$index,saturn$lwr,type="l",lwd=3)
points(jupiter$index,jupiter$lwr,type="l",lwd=3,col="blue")
points(saturn$index,saturn$upr,type="l",lwd=3)
points(jupiter$index,jupiter$upr,type="l",lwd=3,col="blue")

# Identifying High Leverage Points
lev=hat(model.matrix(price3))
plot(lev,pch=19)
abline(3*mean(lev),0,col="red")
price[lev>3*mean(lev),1]

# Prediction Interval
sun1=predict(price3,price,interval = "predict")
max(sun1)
min(sun1)


d <- price
d$predicted <- predict(price3)
d$residuals <- residuals(price3)
d %>% select(price_y,predicted,residuals) %>% head()
ggplot(d, aes(x=horsepower_x1,y=price_y)) +
        geom_smooth(method="lm",se=FALSE,color="lightgrey") +
        geom_segment(aes(xend=horsepower_x1,yend=predicted),alpha=.2) +
        geom_point(aes(alpha=abs(residuals))) +
        guides(alpha=FALSE)+
        geom_point(aes(y=predicted),shape=1) +
        theme_bw()
ggplot(d, aes(x=horsepower_x1,y=price_y)) +
        geom_smooth(method="lm",se=FALSE,color="lightgrey") +
        geom_segment(aes(xend=horsepower_x1,yend=predicted),alpha=.2) +
        geom_point(aes(color=abs(residuals),size=abs(residuals))) +
        scale_color_continuous(low="black",high="red") +
        guides(color=FALSE) +
        geom_point(aes(y=predicted),shape=1) +
        theme_bw()
ggplot(d, aes(x=horsepower_x1,y=price_y)) +
        geom_smooth(method="lm",se=FALSE,color="lightgrey") +
        geom_segment(aes(xend=horsepower_x1,yend=predicted),alpha=.2) +
        geom_point(aes(color=residuals)) +
        scale_color_gradient2(low="black",mid="green",high="red") +
        guides(color=FALSE) +
        geom_point(aes(y=predicted),shape=1) +
        theme_bw()


library(tidyr)
d <- price %>% select(price_y,horsepower_x1,peakrpm_x2,doornumber_x3)
d$predicted <- predict(price3)
d$residuals <- residuals(price3)
head(d)
d %>%
        gather(key="iv",value="x",-price_y,-predicted,-residuals)%>%
        ggplot(aes(x=x,y=price_y)) +
        geom_segment(aes(xend=x,yend=predicted),alpha=.2) +
        geom_point(aes(color=residuals)) +
        scale_color_gradient2(low="black",mid="green",high="red")+
        guides(color=FALSE)+
        geom_point(aes(y=predicted),shape=1)+
        facet_grid(~iv,scales = "free_x")+
        theme_bw()




