# packages
pkgs <- rownames(installed.packages())
if(!"pls" %in% pkgs) install.packages("pls")
if(!"glmnet" %in% pkgs) install.packages("glmnet")
if(!"plotmo" %in% pkgs) install.packages("plotmo")

library(pls)
library(glmnet)
library(plotmo)
library(boot)


# load data
load('SugarData.Rdata')



train <- sugarData
test <- unknownSpectrum
head(train)
dim(train)

dim(test)


colnames(train)

rownames(train)
x <- as.matrix(train[,2:473]) # apparently lasso needs it in matrix form
y <- train$ash
head(x)
dim(y)
x[,1]
# perfom Lasso with cv
lasso <- cv.glmnet(x=x, y=y, alpha=1)
plot(lasso)

# minimum test MSE estimate
lasso$lambda.min

# 1 se test mse estimate
lasso$lambda.1se

y_pred <- predict(lasso, newx=as.matrix(test))
y_pred
# predict the training data
yhat <- predict(lasso, x)

plot(train$ash, yhat, xlab="Observed ash content", 
     ylab="Predicted ash content")
abline(a=0, b=1)

plot_glmnet(lasso$glmnet.fit)

coef(lasso)
rownames(coef(lasso))[which(coef(lasso)!=0)]

#pls

pls_fit <- plsr(formula =ash ~ ., 
                method = "simpls", 
                ncomp = 17, scale = TRUE,
                validation = 'LOO', data = train)
plot(pls_fit, ncomp = 4, asp = 1, line = TRUE)
summary(pls_fit)

plot(RMSEP(pls_fit), legendpos = "topright")

##### BOOTSTRAP

bootstrap_preds <- numeric(1000)
for (i in 1:1000) {
  index <- sample(nrow(train), nrow(train), replace = TRUE)
  pls_fit <- plsr(formula = ash ~ ., 
                  method = "simpls", 
                  ncomp = 5, scale = TRUE, subset=index, data = train)
  bootstrap_preds[i] <- min(predict(pls_fit, newdata=test)[5])
}


dim(bootstrap_preds)
mean(bootstrap_preds)

sortedbootstrap_preds <- sort(bootstrap_preds)
pred <- mean(bootstrap_preds)
lwr <- sortedbootstrap_preds[0.025*length(bootstrap_preds)]
upr <- sortedbootstrap_preds[0.975*length(bootstrap_preds)]

#compare to predicted value + CI bounds
pred                      
lwr                          
upr                          
  
                        
                          
                          