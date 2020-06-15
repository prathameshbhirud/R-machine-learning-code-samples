#import required packages
#install.packages("caret")
#install.packages("ellipse")
#install.packages("e1071")
library(caret)
library(ellipse)
library(e1071)

# read data
dataset = read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"), header = FALSE)
colnames(dataset) = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")

dim(dataset)
str(dataset)

# train-test data split
split_index = createDataPartition(dataset$Species, p = 0.8, list = FALSE)
train_set = dataset[split_index,]
test_set = dataset[-split_index,]

# understand type, structure of datasets
dim(train_set)
dim(test_set)

head(train_set)
head(test_set)

sapply(train_set, class)
levels(train_set$Species)

# summarize the class distribution
percentage <- prop.table(table(train_set$Species)) * 100
cbind(freq = table(train_set$Species), percentage = percentage)


# summarize attribute distributions
summary(train_set)


# Visualize dataset
# split input and output
x <- train_set[, 1:4]
y <- train_set[, 5]

par(mar = c(1, 1, 1, 1))

# boxplot for each attribute on one image
par(mfrow = c(1, 4))
for (i in 1:4) {
    boxplot(x[, i], main = names(iris)[i])
}


# scatterplot matrix
featurePlot(x = x, y = y, plot = "ellipse")

# box and whisker plots for each attribute
featurePlot(x = x, y = y, plot = "box")

# density plots for each attribute by class value
scales <- list(x = list(relation = "free"), y = list(relation = "free"))
featurePlot(x = x, y = y, plot = "density", scales = scales)

# Run algorithms using 10-fold cross validation
control = trainControl(method = "CV", number = 10)
metric = "Accuracy"


# a) linear algorithms
set.seed(7)
fit.lda <- train(Species ~ ., data = train_set, method = "lda", metric = metric, trControl = control)
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(Species ~ ., data = train_set, method = "rpart", metric = metric, trControl = control)
# kNN
set.seed(7)
fit.knn <- train(Species ~ ., data = train_set, method = "knn", metric = metric, trControl = control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(Species ~ ., data = train_set, method = "svmRadial", metric = metric, trControl = control)
# Random Forest
set.seed(7)
fit.rf <- train(Species ~ ., data = train_set, method = "rf", metric = metric, trControl = control)


# summarize accuracy of models
results <- resamples(list(lda = fit.lda, cart = fit.cart, knn = fit.knn, svm = fit.svm, rf = fit.rf))
summary(results)

# compare accuracy of models
dotplot(results)

# summarize Best Model
print(fit.lda)


# estimate skill of LDA on the validation dataset
predictions <- predict(fit.lda, test_set)
confusionMatrix(predictions, test_set$Species)