#read data
data = read.csv("C:/Prathamesh/Sample Codes/R/RTVS-docs-master/examples/datasets/bank/bank-full.csv", sep = ";", stringsAsFactors = FALSE)
head(data)

#checking NA values in any columns if any
colnames(data)[colSums(is.na(data)) > 0]

#structure of data
str(data)

#converting to factor from character
data$job = as.factor(data$job)
data$marital = as.factor(data$marital)
data$education = as.factor(data$education)
data$month = as.factor(data$month)
data$default = as.factor(data$default)
data$loan = as.factor(data$loan)
data$housing = as.factor(data$housing)
data$contact = as.factor(data$contact)
data$poutcome = as.factor(data$poutcome)
data$y = as.factor(data$y)

#structure of data
str(data)

# Split into Train and Validation sets
# Training Set : Validation Set = 70 : 30 (random)
set.seed(100)
train <- sample(nrow(data), 0.7 * nrow(data), replace = FALSE)
TrainSet <- data[train,]
TestSet <- data[-train,]
summary(TrainSet)
summary(TestSet)

#install.packages("randomForest")
library(randomForest)
model = randomForest(y ~ ., data = TrainSet, importance = TRUE)
model

# Fine tuning parameters of Random Forest model
model2 <- randomForest(y ~ ., data = TrainSet, ntree = 500, mtry = 10, importance = TRUE)
model2

# Predicting on train set
predTrain <- predict(model2, TrainSet, type = "class")
# Checking classification accuracy
table(predTrain, TrainSet$y)


# Predicting on Test set
predTest <- predict(model2, TestSet, type = "class")
# Checking classification accuracy
mean(predTest == TestSet$y)
table(predTest, TestSet$y)