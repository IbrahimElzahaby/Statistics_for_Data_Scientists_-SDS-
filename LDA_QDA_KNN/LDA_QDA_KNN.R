pkgs <- rownames(installed.packages())
if(!"MASS" %in% pkgs) install.packages("MASS")
if(!"class" %in% pkgs) install.packages("class")
library(MASS)
library(class)


# Make data points for demo of linear discriminant analysis
# response y, explanatory variable x
# y = 1 indicates a responder and y = 0 a non-responder
# x is the concentration (in mM) of a substrance in blood
# x is sampled from normal distr. with sd = 1 and mean = 6 (group y = 1) or mean = 10 (group y == 2)

# set the seed to fix simulated data
set.seed(35)

# Create data for a large number of patients (the population)
# One explanatory variable x

mu0 <- 6 # Average concentration non-responders to treatment A
mu1 <- 10 # Average concentration responders to treatment A
var_x <- 1 # Variance in log blood concentration within responder and non-responder group
pi0 <- 0.40 # Prior probability non-responder
pi1 <- 0.60 # Prior probability responder
n <- 20000 # Number of patients in population
n0 <- pi0*n # Number of non-responders
n1 <- pi1*n # Number of responders

# Simulated log of blood concentration-values
x0 <- rnorm(n0, mu0, sqrt(var_x)) # Roughly x for non-responders covers 2.5 - 10
x1 <- rnorm(n1, mu1, sqrt(var_x)) # Roughly x for responders covers 5.5 - 14
x <- c(x0,x1)

# Response y
y <- c(rep(0,n0),rep(1,n1))

# Combine in data frame
data_population <- data.frame(cbind(x,y))

# Plot simulated blood concentrations
hist(x0, col = "coral", xlim = c(2,15), ylim = c(0,2500), 
     xlab = "Blood concentration",  main = "")
hist(x1, col = "cornflowerblue", add = TRUE)

# Obtain a random sample of 24 patients from the population
set.seed(5)
sample2 <- data_population[sample(1:20000,24, FALSE),]
hist(sample2$x[sample2$y == 0], col = "coral", xlim = c(2,15), ylim = c(0,5), breaks = 4, xlab = "Blood concentration",  main = "")
hist(sample2$x[sample2$y == 1], col = "cornflowerblue", add = TRUE, breaks = 4)

# Obtain estimates of prior probabilities, means and (pooled) standard deviation by hand
n_class <- table(sample2$y) # Number of observations in each class
(prior_est <- n_class/24) # Divide number of observations in class 1 (or 2) by total number of observations

(mean_est <- aggregate(cbind(mean_x = x) ~ y, data = sample2, FUN = mean)) # Estimate mean of each class 

sd_est <- aggregate(cbind(sd_x = x) ~ y, data = sample2, FUN = sd) # Estimate standard deviation of each class 
(sd_pool_est <- sqrt(((n_class[1]-1)*sd_est$sd_x[1]^2 + (n_class[2]-1)*sd_est$sd_x[2]^2)/(24 - 2))) # Sample estimate of pooled standard deviation

# Compute density for a blood value of 7 for responders and non-responders
(dens_nonresponder <- dnorm(x = 7, mean = mean_est$mean_x[1], sd = sd_pool_est))
(dens_responder <- dnorm(x = 7, mean = mean_est$mean_x[2], sd = sd_pool_est))

# Use lda() function to fit lda model 
(lda.fit <- lda(y ~ x, data = sample2))

# Use predict() to classify new observations
new_sample <- data.frame("x" = c(7))
lda.pred <- predict(lda.fit, newdata = new_sample)

# Load data
pairs(iris[c("Sepal.Length", "Sepal.Width",  "Petal.Length", "Petal.Width")], pch = 19, col = ifelse(iris$Species == 'setosa',"coral", ifelse(iris$Species == 'versicolor',"cornflowerblue","darkgreen")))
par(xpd=TRUE)
legend(0, 0.2, as.vector(unique(iris$Species)),  
       fill=c("coral", "cornflowerblue", "darkgreen"))

# Create train and test data set
set.seed(10)
index<-sample(seq_len(nrow(iris)), size = nrow(iris)*.6)
iris_train<-iris[index,]
iris_test<-iris[-index,]

# Fit LDA model and plot
lda.fit <- lda(Species ~ Sepal.Length + Sepal.Width, data = iris_train)

# For plotting of class boundary:
px1 <- seq(4,8, by = 0.01)
px2 <- seq(1.5,5,by = 0.01)
x_grid <- expand.grid(x=px1, y=px2)  # grid of test samples
colnames(x_grid) <- c("Sepal.Length","Sepal.Width")
grid_pred <-  predict(lda.fit, newdata = x_grid)
grid_pred_mat <- matrix(as.numeric(as.factor(grid_pred$class)), length(px1), length(px2)) # Convert to matrix for plotting
# Plot training data and class boundary
contour(px1, px2, grid_pred_mat, levels=(1:(3-1)+.5), labels="", xlab = "Sepal length", ylab = "Sepal width", 
        axes=TRUE, lwd = 2, lty = 2, col = "purple") 
points(iris_train$Sepal.Length, iris_train$Sepal.Width, col = ifelse(iris_train$Species == 'setosa',"coral", ifelse(iris_train$Species == 'versicolor',"cornflowerblue","darkgreen")))
legend("topright",as.vector(unique(iris$Species)),  
       fill=c("coral", "cornflowerblue", "darkgreen"))

# Fit QDA model and plot
qda.fit <- qda(Species ~ Sepal.Length + Sepal.Width, data = iris_train)
# For plotting of class boundary:
grid_pred <-  predict(qda.fit, newdata = x_grid)
grid_pred_mat <- matrix(as.numeric(as.factor(grid_pred$class)), length(px1), length(px2)) # Convert to matrix for plotting
# Plot training data and class boundary
contour(px1, px2, grid_pred_mat, levels=(1:(3-1)+.5), labels="", xlab = "Sepal length", ylab = "Sepal width", 
        axes=TRUE, lwd = 2, lty = 2, col = "purple") 
points(iris_train$Sepal.Length, iris_train$Sepal.Width, col = ifelse(iris_train$Species == 'setosa',"coral", ifelse(iris_train$Species == 'versicolor',"cornflowerblue","darkgreen")))
legend("topright",as.vector(unique(iris$Species)),  
       fill=c("coral", "cornflowerblue", "darkgreen"))

# Fit KNN model and plot
r <- sapply(iris_train[c("Sepal.Length","Sepal.Width")], range, na.rm = TRUE)
px1 <- seq(r[1,1], r[2,1], length.out = 100)
px2 <- seq(r[1,2], r[2,2], length.out = 100)
g <- cbind(rep(px1, each=100), rep(px2, time = 100))
colnames(g) <- colnames(r)

knn.fit <- knn(iris_train[c("Sepal.Length","Sepal.Width")],g,iris_train$Species,k=15,prob = FALSE) # Investigate k = 1, 15, 50
knn.fit <- as.factor(knn.fit)
z <- matrix(as.integer(knn.fit), nrow = 100, byrow = TRUE)
contour(px1, px2, z, levels=(1:(3-1)+.5), labels="", xlab = "Sepal length", ylab = "Sepal width", 
        axes=TRUE, lwd = 2, lty = 1, col = "purple")
points(iris_train$Sepal.Length, iris_train$Sepal.Width, col = ifelse(iris_train$Species == 'setosa',"coral", ifelse(iris_train$Species == 'versicolor',"cornflowerblue","darkgreen")))

# Fit LDA, QDA and KNN classifiers using all four explanatory variables

## LDA
lda.fit2 <- lda(Species ~ ., data = iris_train)
lda.pred_train <- predict(lda.fit2, newdata = iris_train[,1:4])
lda.pred_test <- predict(lda.fit2, newdata = iris_test[,1:4])
LDA.acc.train <- mean(lda.pred_train$class == iris_train$Species)
LDA.acc.test <- mean(lda.pred_test$class == iris_test$Species)

## QDA
qda.fit2 <- lda(Species ~ ., data = iris_train)
qda.pred_train <- predict(qda.fit2, newdata = iris_train[,1:4])
qda.pred_test<- predict(qda.fit2, newdata = iris_test[,1:4])
QDA.acc.train <- mean(qda.pred_train$class == iris_train$Species) # Prediction accuracy QDA on training data
QDA.acc.test <- mean(qda.pred_test$class == iris_test$Species) # Prediction accuracy QDA on test data

## KNN
k <- 80 # Set number of neighbors
knn.pred_train <- knn(iris_train[,1:4],iris_train[,1:4],iris_train$Species,k=k,prob = FALSE)
knn.pred_test <- knn(iris_train[,1:4],iris_test[,1:4],iris_train$Species,k=k,prob = FALSE)
KNN80.acc.train <- mean(knn.pred_train == iris_train$Species) # Prediction accuracy KNN on training data
KNN80.acc.test<-mean(knn.pred_test == iris_test$Species) # Prediction accuracy KNN on test data

k <- 15 # Set number of neighbors
knn.pred_train <- knn(iris_train[,1:4],iris_train[,1:4],iris_train$Species,k=k,prob = FALSE)
knn.pred_test <- knn(iris_train[,1:4],iris_test[,1:4],iris_train$Species,k=k,prob = FALSE)
KNN15.acc.train <- mean(knn.pred_train == iris_train$Species) # Prediction accuracy KNN on training data
KNN15.acc.test<-mean(knn.pred_test == iris_test$Species) # Prediction accuracy KNN on test data

k <- 1 # Set number of neighbors
knn.pred_train <- knn(iris_train[,1:4],iris_train[,1:4],iris_train$Species,k=k,prob = FALSE)
knn.pred_test <- knn(iris_train[,1:4],iris_test[,1:4],iris_train$Species,k=k,prob = FALSE)
KNN1.acc.train <- mean(knn.pred_train == iris_train$Species) # Prediction accuracy KNN on training data
KNN1.acc.test<-mean(knn.pred_test == iris_test$Species) # Prediction accuracy KNN on test data

# Compute confusion matrix of LDA classifier
table(iris_test$Species,lda.pred_test$class, dnn = c("Actual","Predicted"))

# Load data 
data <- read.csv("1_3_Exercise_3_data.csv")
dim(data) 
# 569 observations and 11 variables
str(data) 
# First 10 variables are the explanatory variables
# Class label is provided in the 11th variable, where class 2 corresponds to "Malignant" and class 1 to "benign"

# Create train and test set
set.seed(10)
index<-sample(seq_len(569), size = 340)
data_train<-data[index,]
data_test<-data[-index,]

# Fit LDA model
lda.fit <- lda(Class ~ ., data = data_train)

# Obtain predictions for test observations
lda.pred <- predict(lda.fit, newdata = data_test)
test_acc <- mean(lda.pred$class == data_test$Class) # Classification accuracy

# Compute confusion matrix for test observations
table(data_test$Class, lda.pred$class, dnn = c("Actual","Predicted"))

# Obtain predictions based on posterior probability for class Malignant using probability cutoff of 0.5
posterior_M <- lda.pred$posterior[,2]
pred_0_5 <- ifelse(posterior_M > 0.5, 2, 1) # Classify as 2 (Malignant) if posterior probability > 0.5, else classify as 1 (Benign)
mean(pred_0_5 == data_test$Class) # Classification accuracy
table(data_test$Class,pred_0_5)

# Obtain predictions based on posterior probability for class Malignant using probability cutoff of 0.5
pred_0_2 <- ifelse(posterior_M > 0.2, 2, 1) # Classify as 2 (Malignant) if posterior probability > 0.5, else classify as 1 (Benign)
mean(pred_0_2 == data_test$Class) # Classification accuracy
table(data_test$Class,pred_0_2, dnn = c("Actual","Predicted"))

# Compute TPR and FPR for a series of probability cut-off values
labels <- data_test$Class - 1 # Define new label vector with values 1 for Malignant and 0 for Benign
labels_ordered <- labels[order(posterior_M, decreasing=TRUE)] # Order labels based on the LDA posterior probability for malignant
ROC_values <- data.frame(TPR=cumsum(labels_ordered)/sum(labels_ordered), FPR=cumsum(!labels_ordered)/sum(!labels_ordered), labels_ordered) # Compute TPR and FPR for a series of probability cut-off points

# Plot ROC curve
plot(ROC_values$FPR, ROC_values$TPR, type = "l", lwd = 2, xlab = "False positive rate", ylab = "True positive rate", main = "ROC curve")










