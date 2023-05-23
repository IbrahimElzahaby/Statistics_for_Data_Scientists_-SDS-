## Packages (install and invoke)
pkgs <- rownames(installed.packages())
if(!"splines" %in% pkgs) install.packages("splines")
if(!"pryr" %in% pkgs) install.packages("pryr")
if(!"mgcv" %in% pkgs) install.packages("mgcv")
if(!"reshape2" %in% pkgs) install.packages("reshape2")
if(!"gridExtra" %in% pkgs) install.packages("gridExtra")


library(splines)
library(pryr)
library(mgcv)
library(leaps) # subset selection
library(reshape2)
library(gridExtra)
library(grid)
library(ggplot2)
# read in the data
maize <- read.csv('maize data SDS assignment v1.csv')

# Explore the data
attach(maize)
#View(maize) # take a look at the data
dim(maize) # shape of the data
summary(maize) # data summary stats

# Visualize data structure

pairs(maize[,c(4:9,15)], panel = panel.smooth) # rules out linear methods
pairs(maize[,c(10:14,15)], panel = panel.smooth) # rules out linear methods


# subset selection


y <- maize$grain.number # grain number
x <-  maize[,c(4:14)] # predictors
mydata <- cbind(y,x) # data frame with response and predictors only.
dim(mydata)
detach(maize)

head(mydata)


#select the best subset
mdl2 <- regsubsets(y ~ . , data=mydata)
mysummary <- summary(mdl2)
print(mysummary$outmat)

#lets find out optimal model complexity
par(mfrow=c(2,3))
plot(mysummary$rsq, type="b", xlab="Number of variables", ylab="R2")
plot(mysummary$adjr2, type="b", xlab="Number of variables", ylab="AdjR2")
plot(mysummary$rss, type="b", xlab="Number of variables", ylab="RSS")
plot(mysummary$cp, type="b", xlab="Number of variables", ylab="Cp")
plot(mysummary$bic, type="b", xlab="Number of variables", ylab="BIC")

 
#seems 3 predictors give the optimal complexity; lets select them
coef(mdl2,3) #select the model with three best subset



# model first the benchmark model
attach(mydata)
fit.lin <- gam(y ~ Tnight.2 + Ri.1 + Psi.2)
fit.lin.summary <- summary(fit.lin)
fit.lin.summary
par(mfrow = c(1,3))
plot(fit.lin, se = TRUE, all.terms = TRUE)

#try a linear model  with our selected predictors
best_subset_fit.lin <- gam(y ~ Tnight.1 + Psi.2 + Tmax.3)
best_subset_fit.lin.summary <- summary(best_subset_fit.lin)
par(mfrow = c(1,3))
plot(best_subset_fit.lin, se = TRUE, all.terms = TRUE)

# let add polynomial terms
best_subset_fit.poly <- gam(y ~ poly(Tnight.1,2) + poly(Psi.2,2) + poly(Tmax.3,2))

best_subset_fit.poly.summary <- summary(best_subset_fit.poly) # small improvement compared to the linear

par(mfrow = c(1,3))
plot(best_subset_fit.poly, se = TRUE, all.terms = TRUE)
par(mfrow = c(2,2))
gam.check(best_subset_fit.poly)

#lets try with 3 poly.
best_subset_fit.poly3 <- gam(y ~ poly(Tnight.1,3) + poly(Psi.2,3) + poly(Tmax.3,3))

best_subset_fit.poly3.summary <- summary(best_subset_fit.poly3) # 

## A GAM with natural cubic splines
fit.ns <- gam(y ~ ns(Tnight.1, df = 4) + ns(Psi.2, df = 4) + ns(Tmax.3, df = 4))
fit.ns.summary <- summary(fit.ns)
par(mfrow = c(1,3))
plot(fit.ns, se = TRUE, all.terms = TRUE)
par(mfrow = c(2,2))
gam.check(fit.ns)


## A GAM with smoothing splines and natural cubic splines
fit.mix <- gam(y ~ ns(Tnight.1, df = 4) + 
                 s(Psi.2,bs = "cr", m = 2) + 
                 s(Tmax.3,bs = "cr", m = 2))
fit.mix.summary <- summary(fit.mix)

par(mfrow = c(1,3))
plot(fit.mix, se = TRUE, shade = TRUE, all.terms = TRUE)
par(mfrow = c(2,2))
gam.check(fit.mix) 

## Model comparison

# extract AIC
aic <- AIC(fit.lin, best_subset_fit.lin, best_subset_fit.poly, best_subset_fit.poly3, fit.ns, fit.mix)[,2] 
#extract BIC

bic <- BIC(fit.lin, best_subset_fit.lin, best_subset_fit.poly, best_subset_fit.poly3, fit.ns, fit.mix)[,2]

#extract degrees of freedom
df <- BIC(fit.lin, best_subset_fit.lin, best_subset_fit.poly, best_subset_fit.poly3, fit.ns, fit.mix)[,1]
#extract adjR2
adjR2 <- c(fit.lin.summary$r.sq, best_subset_fit.lin.summary$r.sq,
           best_subset_fit.poly.summary$r.sq,
           best_subset_fit.poly3.summary$r.sq,
           fit.ns.summary$r.sq, fit.mix.summary$r.sq)
#extract GCV
gcv <- c(fit.lin.summary$sp.criterion, best_subset_fit.lin.summary$sp.criterion,
         best_subset_fit.poly.summary$sp.criterion,
         best_subset_fit.poly3.summary$sp.criterion,
         fit.ns.summary$sp.criterion, fit.mix.summary$sp.criterion)
#extract Scale
scale <- c(fit.lin.summary$scale, best_subset_fit.lin.summary$scale,
           best_subset_fit.poly.summary$scale,
           best_subset_fit.poly3.summary$scale,
           fit.ns.summary$scale, fit.mix.summary$scale)

# make data frame of the criteria

model <- c('bencmark', 'linear',
           'poly2', 'poly3',
           'ns', 'mix')

criteria <- data.frame(model,df, adjR2,gcv,scale,aic,bic)
criteria
#View(criteria)

# Visualize criteria


#plot gcv and scale together

#first: create a data frame of gcv and scale
model <- as.factor(criteria$model)
criteria.name <-  c(rep('gcv',6), rep('scale', 6))
criteria.value <- c(criteria$gcv, criteria$scale)
#criteria.model
df_gcv_scale <- data.frame(criteria.name,criteria.value, model)
#View(df_gcv_scale)

#second: scatter plot
gcv_scale_plot <- ggplot(df_gcv_scale, aes(model, criteria.value)) +
  geom_point(aes(colour = criteria.name), size =5)+
  theme(text = element_text(size = 20))+
  labs(title='Plot of GCV and scale') 

gcv_scale_plot 

#lets plot aic and bic together
#first: create a data frame of aic and bic
criteria.name <- c(rep('aic', 6), rep('bic', 6))
criteria.value <- c(criteria$aic, criteria$bic)
df_aic_bic <- data.frame(criteria.name,criteria.value, model )
#View(df_aic_bic)
aic_bic_plot <- ggplot(df_aic_bic, aes(model, criteria.value)) +
  geom_point(aes(colour = criteria.name), size = 5) +
  theme(text = element_text(size = 20))+
  labs(title='Plot of AIC and BIC') 
  
aic_bic_plot


#plot adjR2
adjR2_plot <- ggplot(criteria, aes(model, adjR2)) +
  geom_point(aes(colour = adjR2), size = 5)+
  theme(text = element_text(size = 20))+
  labs(title='Plot of Adjusted R square')
adjR2_plot

#plot degrees of freedom as a measure of complexity
df_plot <- ggplot(criteria, aes(model, df)) +
  geom_point(aes(colour = df), size = 5) +
  theme(text = element_text(size = 20))+
  labs(title='Plot of degrees of freedom')
df_plot

# arrange the plots
grid.arrange(gcv_scale_plot,aic_bic_plot,  adjR2_plot, df_plot, nrow = 2, 
             top=textGrob('Model selection Criteria'))

#write.csv(criteria, file = 'criteria.csv')













