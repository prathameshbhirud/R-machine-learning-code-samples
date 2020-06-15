install.packages("caret")
install.packages("grDevices")
install.packages("leaps")
install.packages("relaimpo")
install.packages("corrplot")
install.packages("car")
install.packages("DAAG")


library(caret)
library(grDevices)
library(leaps)
library(relaimpo)
library(corrplot)
library(car)
library(DAAG)
# Alters the way numbers are outputted
options(scipen = 999, digits = 3)

#reading data
data = read.csv("C:/Prathamesh/Sample Codes/R/RTVS-docs-master/examples/datasets/forest/forestfires.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
dim(data)

#checking column names
colnames(data)

# Check to see if any data missing - we're OK here so can proceed.
sum(is.na(data))

#check if any outcome variable(Area) is 0 or not
length(which(data$area == 0))

# Convert month and day string variables into numeric values
data$day = as.numeric(as.factor(data$day))
data$month = as.numeric(as.factor(data$month))

# Density plot - Predictors
# Shows us rain is right skewed and FFMC is left skewed. 
# We can see normal looking distributions for temp, wind, RH, X, DMC and day.
#pdf(file="predictordesnisty.pdf")
par(mfrow = c(3, 4), mar = c(3.90, 4.25, 2.5, 0.5))
for (i in 1:(dim(data)[2]-1)) {
    x = data[, i]
    d = density(x)
    plot(d, main = names(data[i]), xlab = "")
    polygon(d, col = "red", border = "blue")
    title("Density plots for all model variables", line = -19, outer = TRUE)
}

# Rain variable has a heavy 0 distribution with only 1.56% of the data
# This will therefore be removed from the model as there is not enough variance
print(paste("Pecentage non-zero rain: ", round(length(which(data$rain > 0)) / dim(data[1]) * 100, 2)))
data = data[, - which(colnames(data) == "rain")]

# Since the FFMC is left-skew, we'll cube it to normalize it
par(mfrow = c(1, 2), mar = c(5, 4.25, 5.5, 2))
d <- density(data$FFMC)
plot(d, main = "FFMC Density (original)", xlab = "FFMC index", col = 'tomato', lwd = 3)
data$FFMC <- (data$FFMC ^ 3)
d <- density(data$FFMC)
plot(d, main = "FFMC Density (x^3)", xlab = "FFMC index", col = 'tomato', lwd = 3)

# Density plot - Outcome
# Shows us a extensive right skew in the data
par(mfrow = c(1, 2), mar = c(5, 4.25, 5.5, 2))
d <- density(data$area)
plot(d, main = "Area Burned Density (original)", xlab = "Area Burned (Hec)", col = 'tomato', lwd = 3)
d <- density(log(data$area + 1))
plot(d, main = "Area Burned Density (log(x+1))", xlab = "Area Burned (Hec)", col = 'tomato', lwd = 3)

# Heavy skew indicates log transformation
# Since there are also many 0 counts for area, we'll first add 1 before transforming
data$area <- log(data$area + 1)

# Examine correlations between all 12 predictors and the area outcome
par(mfrow = c(1, 1))
corr_mat = cor(data)
corrplot(corr_mat, method = "color", outline = TRUE, type = "lower", order = "hclust", tl.col = "black", tl.srt = 45, diag = FALSE,
         tl.cex = 1, mar = c(0, 0, 3, 0), title = "Correlation matrxi between predictor and Outcome variables")


assumptionsmodel <- lm(area ~ ., data = data)
lmtest::bptest(assumptionsmodel)
par(mfrow = c(2, 2))
plot(assumptionsmodel)

assumptionsmodel_all <- lm(area ~ ., data = data)
assumptionsmodel_0 <- lm(area ~ ., data = data[which(data$area > 0),])
# Remove all cases with an area burned of 0
data <- data[which(data$area > 0),]
# Plots both with and without 0 residuals
par(mfrow = c(1, 2))
hist(assumptionsmodel_all$residuals, main = "Data with 0 area burned", xlab = 'Residuals')
abline(v = mean(assumptionsmodel_all$residuals), col = 'red', lwd = 2)
hist(assumptionsmodel_0$residuals, main = "Data without 0 area burned", xlab = 'Residuals')
abline(v = mean(assumptionsmodel_0$residuals), col = 'red', lwd = 2)


model1 <- lm(area ~ ., data = data)
outcome1 <- summary(model1)
(round(outcome1$coefficients[, c(2, 4)], 3))
# Find which variables are significant
sig.pred <- row.names(outcome1$coefficients)[which(outcome1$coefficients[, 4] <= 0.05)]
rsquare <- round(summary(model1)$r.squared, 3)
print(paste("The R2 value is ", rsquare))


# Remove the 2 outliers from the data
data2 <- data[-which(row.names(data) %in% c(200, 470)),]

# Run full model again with outliers removed
model3 <- lm(area ~ ., data = data)
outcome3 <- summary(model3)
# Find which variables are significant
sig.pred <- row.names(outcome3$coefficients)[which(outcome3$coefficients[, 4] <= 0.05)]
rsquare <- round(summary(model3)$r.squared, 3)
print(paste("The R2 value is ", rsquare))


#Perform initial cross validaiton
par(mfrow = c(1, 1))
cv_all <- cv.lm(data = data2, model3, m = 3)


RMSD_origin <- sqrt(mean((cv_all$cvpred - cv_all$area) ^ 2))
print(paste("All variable model RMSD: ", RMSD_origin))

# Loop though all subsets to calucate a model which each as the predictors
subset.out <- regsubsets(area ~ ., data = data2, nbest = 1, nvmax = NULL, method = "exhaustive")
summary.out <- summary(subset.out)
# Returns the variables used in the best model
bestmodelvariables <- summary.out$which[which.max(summary.out$adjr2),]
par(mfrow = c(1, 1))
# PLot all the subsets against the adjusted r2 value
plot(subset.out, scale = "adjr2", main = "All Subset Regression")

variables <- names(bestmodelvariables[which(bestmodelvariables == TRUE)])[-1]
variables

# Extract the best variables and inset into model
formula <- noquote(paste("area~", paste(variables, collapse = '+'), sep = ""))
modelfinal <- lm(formula, data = data2)
# Perform cross-validation on the new model
cv_final <- cv.lm(data = data2, modelfinal, m = 3)


RMSD_final <- sqrt(mean((cv_final$cvpred - cv_final$area) ^ 2))
print(paste("Subset variable model RMSD: ", round(RMSD_final, 2)))

