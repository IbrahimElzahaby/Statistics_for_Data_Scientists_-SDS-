
## Packages (install and invoke)
pkgs <- rownames(installed.packages())
if(!"tree" %in% pkgs) install.packages("tree")
if(!"randomForest" %in% pkgs) install.packages("randomForest")
if(!"gbm" %in% pkgs) install.packages("gbm")
if(!"caret" %in% pkgs) install.packages("caret")
if(!"emmeans" %in% pkgs) install.packages("emmeans")

library(tree)
library(randomForest)
library(gbm)
library(caret)
library(emmeans)
# Load Data

liver_train <- read.csv('indian_liver_train.csv')
liver_test <- read.csv('indian_liver_test.csv')

###############################################
#########Explore Data
###############################################

dim(liver_train)
dim(liver_test)
colnames(liver_train)

#Convert the response to factors
liver_train$Disease <- factor(liver_train$Disease)
liver_test$Disease <- factor(liver_test$Disease)
#convert gender into a factor
liver_train$Gender <- factor(liver_train$Gender)
liver_test$Gender <- factor(liver_test$Gender)


################################################
###### Random Forest
################################################

#######(1) RF; m=p/2
set.seed(1)
?randomForest

rf1 <-randomForest(Disease~., data=liver_train,
                      importance = TRUE, mtry=2,ntree = 500)
rf1_preds<-predict(rf1, newdata=liver_test)


rf1$confusion # oob confusion matrix

  
#confusion matrix; more detailed
rf1_preds <- factor(rf1_preds)
rf1_confusion <- confusionMatrix(rf1_preds, liver_test$Disease)

rf1_confusion
?fourfoldplot
fourfoldplot(rf1_confusion$table, conf.level = 0.95, color = c('red', 'green'), main = 'Random forest confusion matrix; m=p/2 ')

#######(2) RF; m=(p)^1/2

set.seed(1)
?randomForest

rf2 <-randomForest(Disease~., data=liver_train,
                   importance = TRUE,ntree = 500, mtry=sqrt(ncol(liver_train)-1)) #minus cause the response is not a predictor
rf2_preds<-predict(rf2, newdata=liver_test)


rf2$confusion # confusion matrix and class error rate

#confusion matrix; more detailed
rf2_preds <- factor(rf2_preds)
rf2_confusion <- confusionMatrix(rf2_preds, liver_test$Disease)
rf2_confusion
fourfoldplot(rf2_confusion$table, conf.level = 0.95, color = c('red', 'green'), main = 'Random forest confusion matrix; m=(p)^1/2 ')





###############################################################################
#############  Bagging 
###############################################################################
set.seed(1)
bag <-randomForest(Disease~., data=liver_train,
                   importance = TRUE,ntree = 500, mtry=10) #minus cause the response is not a predictor
bag_preds<-predict(bag, newdata=liver_test)

#which(rf1_preds != liver_test$Disease)
bag$confusion # confusion matrix and class error rate

#confusion matrix; more detailed
bag_preds <- factor(bag_preds)
bag_confusion <- confusionMatrix(bag_preds, liver_test$Disease)
bag_confusion
fourfoldplot(bag_confusion$table, conf.level = 0.95, color = c('red', 'green'), main = 'Bagging confusion matrix')




###############################################################################
####   plot OOB
###############################################################################
par(mfrow=c(2,2))
plot(rf1$err.rate[,1], main = ' RF; m=p/2 OOB error', xlab = 'No. of trees', ylab = 'OOB error') ## oob for RF; m=p/2
plot(rf2$err.rate[,1], main = '  RF; m=(p)^1/2 OOB error' ,xlab = 'No. of trees', ylab = 'OOB error') ## oob for RF; m=(p)^1/2
plot(bag$err.rate[,1], main = ' Bagging OOB error', xlab = 'No. of trees', ylab = 'OOB error') ## oob for bagging
rf1$err.rate
dev.off()

###############################################################################
####   Boosting
###############################################################################

# Load Data
set.seed(1)
liver_train <- read.csv('indian_liver_train.csv')
liver_test <- read.csv('indian_liver_test.csv')

#convert gender into a factor
liver_train$Gender <- factor(liver_train$Gender)
liver_test$Gender <- factor(liver_test$Gender)

?gbm
boosting <-gbm(Disease~., data=liver_train, distribution = "bernoulli", shrinkage = 0.01, interaction.depth = 4,
                 cv.folds = 5, keep.data = TRUE, n.trees = 500)

plot(boosting$cv.error, xlab = 'No. of trees', ylab = 'OOB Error', main = 'Boosting OOB Error')

###############################################################################
####   Boosting prediction using 100 trees
###############################################################################


boosting_preds <- predict.gbm(boosting, newdata = liver_test, n.trees = 100, type = 'response')
length(boosting_preds)

boosting_response <- ifelse(boosting_preds> 0.5,1,0)
length(boosting_response)

boosting_response <- factor(boosting_response)
liver_test$Disease <- factor(liver_test$Disease)

boosting_confusion <- confusionMatrix(boosting_response, liver_test$Disease)
boosting_confusion
fourfoldplot(boosting_confusion$table, conf.level = 0.95, color = c('red', 'green'), main = 'Boosting confusion matrix')

###############################################################################
####   Boosting prediction using 200 trees
###############################################################################


boosting_preds <- predict.gbm(boosting, newdata = liver_test, n.trees = 200, type = 'response')
length(boosting_preds)

boosting_response <- ifelse(boosting_preds> 0.5,1,0)
length(boosting_response)

boosting_response <- factor(boosting_response)
liver_test$Disease <- factor(liver_test$Disease)

boosting_confusion <- confusionMatrix(boosting_response, liver_test$Disease)
boosting_confusion


###############################################################################
####   Relative Influence of predictors
###############################################################################

### RF 1
varImpPlot(rf1, main = 'RF; m=p/2 variable Importance')

### RF 2
varImpPlot(rf2, main = 'RF; m=(p)^1/2 variable Importance')

### bagging
varImpPlot(bag, main = 'Bagging variable Importance')
dev.off()
### boosting
summary(boosting, main= 'Boosting Variable importance')

###############################################################################
####   Logistic regression
###############################################################################

## Using all predictors

lg_model <- glm(Disease~., data = liver_train, family = 'binomial')
summary(lg_model)
lg_preds <- predict(lg_model, liver_test, type = 'response')
lg_preds
lg_response <- ifelse(lg_preds> 0.5,1,0)
#confusion matrix
lg_response <- factor(lg_response)

log_confusion <- confusionMatrix(lg_response, liver_test$Disease)
log_confusion
fourfoldplot(log_confusion$table, conf.level = 0.95, color = c('red', 'green'), main = 'logistic Regression confusion matrix')



