library(caret)

training <- read.csv('pml-training.csv')
test <- read.csv('pml-testing.csv')

str(training)
summary(training)

### Transforming all factor variables to numeric (Even though this will cause a lot o NA's)

for (i in 7:ncol(training)-1){
  if (is.factor(training[,i])){
    training[,i] <- as.numeric(levels(training[,i]))[training[,i]]
  }
}

summary(training)

### Removing all variables that contain at least one missing value
aux = vector('numeric')

for (i in 1:ncol(training)) {
  aux <- c(aux,sum(is.na(training[,i]))) 
}

#dlt <- aux != nrow(training)
dlt <- aux == 0

training <- training[, dlt == TRUE]

### Fitting the first model (Classification tree with all variables - Uses bootstrap for cross-validation)

set.seed(140516)

cv_set <- trainControl(method = "cv", number = 10, repeats = 25)

tree_fit <- train(training$classe ~., method = "rpart", trControl = cv_set ,data = training[,-c(1:6)])
tree_fit
acc_fit <- tree_fit$results[1,c(2:ncol(tree_fit$results))]

### Fitting the second model (Gradient boosted tree)

gbm_fit <- train(training$classe ~., method = "gbm", trControl = cv_set, data = training[,-c(1:6)])
gbm_fit
gbm_fit$results
acc_fit <- rbind(acc_fit, gbm_fit$results[9,c(5:ncol(gbm_fit$results))])

### Fitting the third model (Random forest)

rforest_fit <- train(training$classe ~., method = "rf", trControl = cv_set, data = training[,-c(1:6)])
rforest_fit
acc_fit <- rbind(acc_fit, rforest_fit$results[1,c(2:ncol(rforest_fit$results))])


rownames(acc_fit) <- c("Decision Tree", "Gradient boosted tree", "Random forest")

acc_fit
