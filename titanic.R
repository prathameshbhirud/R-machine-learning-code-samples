#install.packages("naniar")
#install.packages("mice")
#install.packages("caTools")

library(dplyr) #data manipulation
library(ggplot2) #visualization
library(naniar) #missing value visualization
library(mice) #missing value imputation
library(randomForest) #classification algorithm
library(rpart) #classification algorithm
library(rpart.plot) #decision tree visualization
library(caTools) #splitting data into training and test set


# read data
titanic_dataset = read.csv(url("https://raw.githubusercontent.com/datasciencedojo/datasets/master/titanic.csv"), header = TRUE, stringsAsFactors = FALSE)

dim(titanic_dataset)
str(titanic_dataset)

summary(titanic_dataset)

# missing value
vis_miss(titanic_dataset)

# visualize Age vs Survival histogram
ggplot(data = titanic_dataset, aes(x = Age, fill = factor(Survived))) +
    geom_histogram(bins = 30) +
    facet_grid(. ~ Sex) +
    ggtitle(" Age vs Survived") +
    scale_fill_discrete(name = "Survived")

# Visualize Survival vs Sex bar graphs
ggplot(titanic_dataset, aes(x = Sex, fill = factor(Survived))) +
    geom_bar(position = 'fill') +
    xlab("Sex") +
    ylab("Frequency") +
    scale_fill_discrete(name = "Survived") +
    ggtitle("Sex vs Survived")

# Visualize Survival vs Pclass bar graphs
ggplot(titanic_dataset, aes(x = Pclass, fill = factor(Survived))) +
    geom_bar(position = "fill") +
    ylab("Frequency") +
    scale_fill_discrete(name = "Survived") +
    ggtitle("Pclass vs Survived")

# Visualize Survival vs Fare bar graphs
ggplot(titanic_dataset, aes(x = Fare, fill = factor(Survived))) +
    geom_histogram() +
    ggtitle("Fare vs survived")


# Deriving Titles
head(titanic_dataset$Name)

# grab title from passenger's name
titanic_dataset$Title = gsub("(.*,)|(\\..*)", '', titanic_dataset$Name)
unique(titanic_dataset$Title)
table(titanic_dataset$Sex, titanic_dataset$Title)

# rectifying typos
titanic_dataset$Title[titanic_dataset$Title == ' Mlle'] = ' Miss'
titanic_dataset$Title[titanic_dataset$Title == ' Ms'] = ' Miss'
titanic_dataset$Title[titanic_dataset$Title == ' Mme'] = ' Mrs'
table(titanic_dataset$Sex, titanic_dataset$Title)

# assign Rare Title to titles which are not frequently used
rare_titles = c(" Don", " Rev", " Col", " Capt", " the Countess", " Jonkheer", " Dr", " Major", " Lady", " Sir")
titanic_dataset$Title[titanic_dataset$Title %in% rare_titles] = " Rare Title"
table(titanic_dataset$Sex, titanic_dataset$Title)

# Visualize Suv=rvival vs Title
ggplot(data = titanic_dataset, aes(x = factor(Title), fill = factor(Survived))) +
    geom_bar() +
    xlab("Title") +
    scale_fill_discrete(name = "Survived") +
    ggtitle("Title vs Survived")

# Deriving Family Size
titanic_dataset$Family_Size = titanic_dataset$SibSp + titanic_dataset$Parch + 1

# Visualize Family Size and Survival
ggplot(titanic_dataset, aes(x = Family_Size, fill = factor(Survived))) +
    geom_bar() +
    scale_x_continuous() +
    labs(x = "Family Size")


# Missing Value Imputation
sum(titanic_dataset$Cabin == "") # missing values in Cabin

sum(titanic_dataset$Embarked == "") # missing values in Embarked
unique(titanic_dataset$Embarked)
which(titanic_dataset$Embarked == "") # gets us records with missing value in asked column

# filter those records out where Embarked contains missing value
fare_embark_data = filter(titanic_dataset, PassengerId %in% which(titanic_dataset$Embarked != ""))
str(fare_embark_data)


ggplot(fare_embark_data, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
    geom_boxplot() +
    geom_line(aes(yintercept = 80))

# As we can see from the graph, The median fare for 1 st class passenger departing from Charbourg(‘C’) coincides nicely 
# with our missing embarkment passengers. Hence, For Pclass 1 and fare$80, the closest option is Embarkment port ‘C’.

#imputing missing values for Embarked
titanic_dataset$Embarked[c(62, 830)] <- 'C'
titanic_dataset[c(62, 830),]

# Exploring Age variable
sum(is.na(titanic_dataset$Age)) # 177 missing age values

#Factorizing variables
titanic_dataset$Pclass <- factor(titanic_dataset$Pclass)
titanic_dataset$Sex <- factor(titanic_dataset$Sex)
titanic_dataset$Embarked <- factor(titanic_dataset$Embarked)
titanic_dataset$Title <- factor(titanic_dataset$Title)
titanic_dataset$Family_Size <- factor(titanic_dataset$Family_Size)
titanic_dataset$Survived <- factor(titanic_dataset$Survived)

#Set a random seed
set.seed(128)
mice_model <- mice(titanic_dataset[, names(titanic_dataset) %in% c('Pclass', 'Sex', 'Embarked', 'Title', 'Age', 'SibSp', 'Parch', 'Fare')], method = 'rf')

mice_output <- complete(mice_model)
par(mfrow = c(1, 2))
hist(titanic_dataset$Age, freq = F, main = "Original data : Age", col = "dark blue", ylim = c(0, 0.04))
hist(mice_output$Age, freq = F, main = "MICE output : Age", col = "blue", ylim = c(0, 0.04))

titanic_dataset$Age <- mice_output$Age

#Checking for missing values now after MICE model
sum(is.na(titanic_dataset$Age))

vis_miss(titanic_dataset)


# Splitting Data into Training and Test set

#Keeping only desired variables for model building
#Age,Survived,Pclass,Sex,Fare,Familysize,Embarked and Title
titanic_sub <- titanic_dataset[, c(2, 3, 5, 6, 10, 12, 13, 14)]

#Set a random seed
set.seed(123)
split = sample.split(titanic_sub$Survived, SplitRatio = 0.75)
training_set = subset(titanic_sub, split == TRUE)
test_set = subset(titanic_sub, split == FALSE)
str(training_set)


# Model Building
# Decision tree Classification
decision_tree_model = rpart(Survived ~ ., data = training_set, method = "class")
pred_survived = predict(decision_tree_model, newdata = test_set[-1], type = "class")

# Confusion Matrix
table(test_set$Survived, pred_survived)

# Accuracy
mean(test_set$Survived == pred_survived)

# Plot tree
rpart.plot(decision_tree_model)
plotcp(decision_tree_model)

# Random forest Classification
set.seed(123)
rf_model <- randomForest(Survived ~ ., data = training_set)
#prediction
rf_pred <- predict(rf_model, test_set)
mean(test_set$Survived == rf_pred)
print(rf_model)

# Plot
plot(rf_model)
legend('topright', colnames(rf_model$err.rate), col = 1:3, fill = 1:3)

varImpPlot(rf_model, main = "RF_Model")

# relative variable importance by plotting the mean decrease
importance <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance),
                            Importance = round(importance[, 'MeanDecreaseGini'], 2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
    mutate(Rank = paste0('#', dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance),
                           y = Importance, fill = Importance)) +
                           geom_bar(stat = 'identity') +
                           geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust = 0, vjust = 0.55, size = 4, colour = 'white') +
            labs(x = 'Variables') +
            coord_flip()

# Final prediction
test_set$pred_survived <- rf_pred
head(test_set)