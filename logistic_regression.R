pkgs <- rownames(install.packages())
if(!"car" %in% pkgs) install.packages("car")
if(!"doBy" %in% pkgs) install.packages("doBy")
if(!"emmeans" %in% pkgs) install.packages("emmeans")
if(!"ggplot2" %in% pkgs) install.packages("ggplot2") 
library(car)
library(doBy)
library(emmeans)
library(ggplot2)
Data1 <- read.csv("Hue_Chlorophyll.csv")
plot (Data1$chlorophyll_transfer ~ Data1$Hue_angle,
     xlab = "Hue angle",
     ylab = "Chlorophyll transfer")
model1 <- lm(chlorophyll_transfer ~ Hue_angle, data = Data1)
summary(model1)
anova(model1)
## Calculate 95% CI of the slope
confint(model1, level = 0.95)
## plot regression line through scatter plot
plot(Data1$chlorophyll_transfer ~ Data1$Hue_angle,
     xlab = "Hue angle",
     ylab = "Chlorophyll transfer")
abline(model1, col = "red")
## Save residuals and predictions
resi <- residuals(model1)
pred <- fitted(model1)
## Arrange for two plots to be shown next to each other
par(mfrow = c(1,2))
## Scatter plot of residual vs predicted values
plot(resi ~ pred)
abline(h=0)
title ("Homoscedasticity check")
## Producing a normal Q-Q plot
qqnorm(resi)
qqline(resi)
## Predictions with se 's and the 95% confidence intervals
## Predictions with se 's and the 95% prediction intervals
newdata <- data.frame(Hue_angle= c(900,1000,1100,1200))
predict(model1,newdata , se.fit = TRUE, interval = "confidence")
predict(model1,newdata , se.fit = TRUE, interval = "prediction")
## Fit model without potential outlier and compare the results
Data1$chlorophyll_transfer[106] <- NA
model2 <- lm(chlorophyll_transfer ~ Hue_angle, data = Data1)
summary(model2)
anova(model2)
## Make plot
par(mfrow = c(1,1))
plot(Data1$chlorophyll_transfer ~ Data1$Hue_angle,
     xlab = "Hue angle",
     ylab = "Chlorophyll transfer")
abline(model2, col = "red")
################################################
Norway_NO2 <- read.csv("Norway_NO2.csv")
## Matrix of scatter plots
pairs(Norway_NO2[,1:4])
pairs(Norway_NO2[,c(1,5,6,7,8)])
## Perform a simple linear regression
lm1 <- lm(y ~ x3, data = Norway_NO2)
summary(lm1)
anova(lm1)
lmmulti <- lm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7, data = Norway_NO2)
summary(lmmulti)
anova(lmmulti)
## Predict the concentration of NO2 at a wind speed of ...
par(mfrow = c(1,1))
plot(y ~ x3, data=Norway_NO2)
# newdata <-data.frame(x3= c(......))
newdata <- data.frame(x1 = mean(Norway_NO2$x1), x2 = mean(Norway_NO2$x2),
                      x3 = c(2,4,6,8),
                      x4 = mean(Norway_NO2$x4), x5 = mean(Norway_NO2$x5),
                      x6 = mean(Norway_NO2$x6), x7 = mean(Norway_NO2$x7))
predict(lmmulti,newdata , se.fit = TRUE, interval = "confidence")









