pkgs <- rownames(installed.packages())
if(!"emmeans" %in% pkgs) install.packages("emmeans")
library(emmeans)
# make 25 binary data points for demo of logistic regression
# response y, explanatory variable x
# x is ln(concentration), here sampled from normal distr. mean=0, sd=1
# logit link, linear predictor = beta0 + beta1 * x

#set the seed to fix simulated data
set.seed(321835)

# create explanatory variable x, linear predictor lp, probabilities p
x<-rnorm(25,0,1)   # roughly, x covers range from -2 to 2
lp <- -1+3*x         # linear predictor with true intercept beta0 and slope beta1
p<- 1/(1+exp((-1)*lp)) # probabilities p according to inverse of logit link
hist(x)

# create data y
set.seed(215241)
r<- runif(25,0,1) # 25 random numbers between 0 and 1
y<- floor(p-r+1) # binary data, Bernouilli distr., probabilities p
floor(z)
r
y
(cbind(x,lp,p,y))

#plotting the data
plot(x,y,col="green",ylab='y,b')
curve(1/(1+exp((-1)*(-1+3*x))),add=T, col="red")
legend("topleft", legend=c("green = data","red     = true curve"))

# first ordinary linear regression (wrong model)
# fitted curve is a line
ordinaryregression <- lm(y~x)
fitted_probability_regression<-fitted.values(ordinaryregression)
plot(x,fitted_probability_regression)

# next logistic regression (better choice of model)
# a sigmoid curve is fitted
oneminusy <- 1-y
logisticregression <- glm(cbind(y,oneminusy)~x,binomial)
# plot the fitted sigmoid
beta <- coefficients(logisticregression)
beta0 <- beta[1]   # the estimated intercept beta0
beta1 <- beta[2]   # the estimated slope beta1
sebetal<- summary(logisticregression)$coefficients[2,2]
plot(x,y,col="green",ylab = 'y,p')#,yaxt='n')
curve(1/(1+exp((-1)*(beta0+beta1*x))),add=T, col="blue")
curve(1/(1+exp((-1)*(-1+3*x))),add = T, col="red")
legend("topleft", legend = c("green = data y","red     = true curve","blue     = fitted curve"))

# Summary
summary(logisticregression)

# z-test (Wald test) for slope beta1 can be taken from summary table
# rough and ready 95% confidence interval beta1 from z-test (Wald test)
# lower and upper bound rr2.5 and rr97.5 respectively
rr2.5 <- beta1-1.96*sebetal
rr97.5<- beta1+1.96*sebetal
(cbind(rr2.5, rr97.5))

# generally better is likelihood ratio 95% confidence interval (default)
confint(logisticregression)

# make 75 binary data points for demo of logistic regression 
# with quantitative and qualitative explanatory variable
# response y, explanatory variable x, factor Group for 3 groups
# x is ln(concentration), here sampled from normal distr. mean=0, sd=1
# Group represent groups w.r.t. healthy lifestyle (low, average, high, or 1, 2, 3)
# y = suitable for treatemnt (1 = yes, 0 = no)
# p = probability for a patient to be suitable for treatment, p = P(y=1|x,group)
# logit link, linear predictor = beta0 + Group + beta1 * x
# so, systematic part of 'true' model consists of three parallel sigmoid curves

# set the seed to fix data that are simulated afterwards
set.seed(321835)

# create explanatory variable x, linear predictor lp, probabilities p
group <- c(rep(1,25),rep(2,25),rep(3,25))
x<-rnorm(75,0,1)   # roughly, 75 values of x cover range from -2 to 2
lp <- group-2+3*x     # true intercepts -1,0,1, true slope = 3
p<-1/(1+exp((-1)*lp)) # probabilities p according to inverse of logit link

# create data y
# again set seed to fix data that are simulated afterwards
set.seed(215241)
r<-runif(75,0,1)   # 75 random numbers between 0 and 1
y<-floor(p-r+1)    # binary data, Bernouilli distr., probabilities p

# plotting the data (together with true parallel curves)
plot(x,y,col=group,pch=group,ylab='y,p')
curve(1/(1+exp((-1)*(-1+3*x))),add = T, col="blue")
curve(1/(1+exp((-1)*(0+3*x))),add = T, col="blue")
curve(1/(1+exp((-1)*(1+3*x))),add = T, col="blue")
legend("topleft", legend = c("green,red,black = data","blue = true curves"))

# logistic regression
# including main effects and interactions for groups
# a factor Group with three levels is made
# first three non-parallel sigmoid curves are fitted
# F added to model name to indicate 'full' model
# model without interaction involves parallel sigmoid curves
# R added to 'reduced' model without interaction
# likelihood ratio test for interaction employing anova command
# P-value from approximate chi-square distribution
Group <- factor(group)
oneminusy <- 1-y
logisticregressionF <- glm(cbind(y,oneminusy)~Group+x+Group:x,binomial)

# reduce model to main effects and test for interaction
logisticregressionR <- glm(cbind(y,oneminusy)~Group+x,binomial)
anova(logisticregressionR,logisticregressionF, test = "Chisq")

# use RR to indicate further model reduction, i.e. dropping interaction and groups
logisticregressionRR <- glm(cbind(y,oneminusy)~x,binomial)
anova(logisticregressionRR,logisticregressionR, test = "Chisq")

# sometimes convenient for interpretation to subtract mean of x from x
# intercepts are population means at average value of x
z <- x-mean(x)
logisticregressionRz <- glm(cbind(y,oneminusy)~Group+z,binomial())
summary(logisticregressionRz)

# follow up by pairwise comparisons between groups
# basically Fisher's LSD (no adjustment for multiple testing) with z-test (Wald test)
compareGroups <- emmeans(logisticregressionRz,pairwise~Group)
summary(compareGroups,adjust="none")
