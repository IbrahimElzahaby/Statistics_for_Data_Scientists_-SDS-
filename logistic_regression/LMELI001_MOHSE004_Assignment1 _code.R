#Authors: Ibrahim Elzahaby and Luka Lmelias



## install libries if they dont exist
## Packages (install and invoke)
pkgs <- rownames(installed.packages())
if(!"emmeans" %in% pkgs) install.packages("emmeans")

# load libraries
library(emmeans)


### Read data

data <- read.csv("botrytis.csv")


#Explore the data
head(data)
#View(data)
#as.numeric(data$cultivar)

# Convert cultivar and ozne to factors

data$cultivar <- factor(data$Cultivar)
data$ozone <- factor(data$Ozone)

data$Ozone
summary(data)
data$cultivar



# Investigate the effect of ozone and cultivar (including interaction) on the infection,

# full model
lgF <- glm(cbind(Infected, 1-Infected)~cultivar+ ozone+ cultivar:ozone, data = data, binomial)

summary(lgF)


## reduced model without interactions

lgR <- glm(cbind(Infected, 1-Infected)~cultivar+ ozone, data = data, binomial)
summary(lgR)

## test with anova
anova(lgR, lgF, test = 'Chisq')

## Test if there is an effect of cultivar by comparing a full and reduced model.

lgF <- glm(cbind(Infected, 1-Infected)~cultivar+ ozone+ cultivar:ozone, data = data, binomial)
lgR <- glm(cbind(Infected, 1-Infected)~ozone, data = data, binomial)
anova(lgR, lgF, test = 'Chisq')


## Test effect of ozone
lgF <- glm(cbind(Infected, 1-Infected)~Ozone+cultivar + ozone:cultivar, data = data, binomial)
lgR <- glm(cbind(Infected, 1-Infected)~cultivar, data = data, binomial)
anova(lgR, lgF, test = 'Chisq')


## fit a model with ozone only

lg <- glm(cbind(Infected, 1-Infected)~Ozone, data = data, binomial)

summary(lg)


### plot infected vs ozone
plot(data$Infected~as.numeric(data$ozone), col=data$cultivar)


# Plot of fitted sigmoid
beta <- coefficients(lg)
beta0 <- beta[1]   # the estimated intercept beta0
beta1 <- beta[2]   # the estimated slope beta1
sebeta1<-summary(lg)$coefficients[2,2]
plot(data$Ozone,data$Infected,
     col="green",ylab='y, p',
     xlab = 'ozone')#,yaxt='n')
curve(1/(1+exp((-1)*(beta0+beta1*x))),add=T, col="blue")
#curve(1/(1+exp((-1)*(data$Infected))),add=T, col="red") 
legend("topleft", legend="fitted curve")

# investigate why the courve is not zero at ozone zero or one at ozone 270
data_ozone0 <- data[which(data$Ozone==0),]
data_ozone270 <- data[which(data$ozone==270),]
which(data_ozone0$Infected==1)
which(data_ozone270$Infected==0)


