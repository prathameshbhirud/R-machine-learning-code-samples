# Data Source: https://archive.ics.uci.edu/ml/machine-learning-databases/car/

install.packages("randomForest")
library(randomForest)

#Load dataset and explore
data1 = read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data"), header = TRUE)
head(data1)

names(data1) = c('BuyingPrice','Maintenance','NumDoors','NumPersons','BootSpace','Safety','Condition')

#split into train and validation sets - 70:30
set.seed(100)
train = sample(nrow(data1), 0.7 * nrow(data1), replace = FALSE)
train_set = data1[train,]
valid_set = data1[-train,]

summary(train_set)
summary(valid_set)



#create random forest model
model1 = randomForest(Condition ~ ., data = train_set, importance = TRUE)
model1

#fine tuning model
model2 = randomForest(Condition ~ ., data = train_set, ntree = 500, mtry = 6, importance = TRUE)
model2

# Predicting on train set
predTrain <- predict(model2, train_set, type = "class")
# Checking classification accuracy
table(predTrain, train_set$Condition)


# Predicting on Validation set
predValid <- predict(model2, valid_set, type = "class")
# Checking classification accuracy
mean(predValid == valid_set$Condition)
table(predValid, valid_set$Condition)

# To check important variables
importance(model2)
varImpPlot(model2)

# Using For loop to identify the right mtry for model
a = c()
i = 5
for (i in 3:8) {
    model3 <- randomForest(Condition ~ ., data = train_set, ntree = 500, mtry = i, importance = TRUE)
    predValid <- predict(model3, valid_set, type = "class")
    a[i - 2] = mean(predValid == valid_set$Condition)
}

a

plot(3:8, a)