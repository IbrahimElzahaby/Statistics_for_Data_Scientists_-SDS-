# Package loadings
pkgs <- rownames(installed.packages())
if(!"pls" %in% pkgs) install.packages("pls")
if(!"glmnet" %in% pkgs) install.packages("glmnet")
if(!"plotmo" %in% pkgs) install.packages("plotmo")

library(pls)
library(glmnet)
library(plotmo)

train <- sugarData
test <- unknownSpectrum
head(train)
dim(test)
colnames(train)
rownames(train)
View(unknownSpectrum)

x <- as.matrix(train[,2:473 ])
y <- train$ash
head(x)
dim(y)
x[,1]
# lasso with CV
lasso <- cv.glmnet(x=x, y=train$ash, alpha=1)
plot(lasso)
# Minimum test MSE estimates
lasso$lambda.1se
y_pred <- predict(lasso, newx=as.matrix(test))
y_pred
# Predict the training data
yhat <- predict(lasso, x)

plot(train$ash, yhat, xlab="Observed ash content",
     ylab="Predicted ash content")
abline(a=0, b=1)
plot_glmnet(lasso$glmnet.fit)
coef(lasso)
rownames(coef(lasso))[which(coef(lasso)!=0)]
#pls
pls_fit <- plsr(formula = train$ash ~ as.matrix(train[,2:473]),
                method = "simpls",
                ncomp = 17, scale = TRUE,
                validation = 'LOO')
plot(pls_fit, ncomp = 4, asp = 1, line = TRUE)
summary(pls_fit)

plot(RMSEP(pls_fit), legendpos = "topright")##### BOOTSTRAP
bootstrap_preds <- numeric(1000)
for (i in 1:1000) {
  index <- sample(nrow(train), nrow(train), replace = TRUE)
  pls_fit <- plsr(formula = ash ~ .,
                  method = "simpls",
                  ncomp = 17, scale = TRUE, subset=index, data = train)
 bootstrap_preds[i] <- min(predict(pls_fit, newdata=test)[5])
}#boot.ci(bootstrap_preds, conf = 0.95, type = 'all')min(bootstrap_preds)dim(bootstrap_preds)
bootstrap_preds
sortedbootstrap_preds <- sort(bootstrap_preds)
pred <- mean(bootstrap_preds)
lwr <- sortedbootstrap_preds[0.025*length(bootstrap_preds)]
upr <- sortedbootstrap_preds[0.975*length(bootstrap_preds)]#compare to predicted value + CI bounds
pred
lwr
upr


#bootstrap_preds <- numeric(1000)
#for (i in 1:1000) {
 # index <- sample(nrow(train), nrow(train), replace = TRUE)
  #mdl <- lm(ash ~ ., data=train[index,])
 # bootstrap_preds[i] <- predict(mdl, newdata=data.frame(hp=324.5 , hp2=99^2))
#}

#mean(bootstrap_preds)

#sortedbootstrap_preds <- sort(bootstrap_preds)
#pred <- mean(bootstrap_preds)
#lwr <- sortedbootstrap_preds[0.025*length(bootstrap_preds)]
#upr <- sortedbootstrap_preds[0.975*length(bootstrap_preds)]

#compare to predicted value + CI bounds
#print(yhat)
#print(data.frame(fit=pred, lwr=lwr, upr=upr))




