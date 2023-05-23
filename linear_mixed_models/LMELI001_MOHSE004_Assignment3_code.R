# Course Statistics for Data Scientists 2021-2022
# Case Study Linear Mixed Models

## Packages (install and invoke)
pkgs <- rownames(installed.packages())
if(!"lme4" %in% pkgs) install.packages("lme4")
if(!"pbkrtest" %in% pkgs) install.packages("pbkrtest")
if(!"emmeans" %in% pkgs) install.packages("emmeans")
if(!"ISLR" %in% pkgs) install.packages("ISLR")
if(!"glmnet" %in% pkgs) install.packages("glmnet")
if(!"rrBLUP" %in% pkgs) install.packages("rrBLUP")

# load librires
library(lme4)
library(pbkrtest)
library(emmeans)
library(ISLR)
library(glmnet)
library(rrBLUP)


#reading the data from file
mydata<-read.table('DATARationSplitPlot.txt',header=T)
mydata
attach(mydata)

# changing variates to factors
Pair<-factor(pair)
Ration<-factor(ration)
Sex<-factor(sex,labels=c('male','female'))

### continuation
summary(mydata)

# ANOVA
# We start anova without the dependent variable 'the wrong way'
lmmanova_excld_pair <-aov(y~Ration+sex+Ration:sex)
summary(lmmanova_excld_pair)


# now account for random effects a.k.a the right way; 
lmmanova<-aov(y~Ration+sex+Ration:sex+Error(Pair))
summary(lmmanova)

#Estimate the variance component of ANOVA

MSpair <- 7.60 # obtained from ANOVA
MSE <- 0.0641 #obtained from ANOVA

variance_pair <- (7.60- 0.0641)/2 #(MSpair-MSE)/2
variance_pair
# REML
reml<-lmer(y~Ration+sex+Ration:sex+(1|Pair))
summary(reml)

reml1<-lmer(y~Ration+sex+(1|Pair))
summary(reml1)


# compare ration:sex interaction
library(pbkrtest)
kr<-KRmodcomp(reml,reml1)
summary(kr)



#compair the means
library(emmeans)
comparevar<-emmeans(reml1,pairwise~Ration, adjust="none") 
summary(comparevar)

# the pairwise comparisons between sex
comparefert<-emmeans(reml1,pairwise~sex, adjust="none")
summary(comparefert)



