library(tidyverse)
install.packages("caret")
library(caret)
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
install.packages("ROCR")
library(ROCR)

#Task 1 
data <- read.csv(file.choose())

colnames(data)
colnames(data) <- c("success", "yardline", "team", "season", "week", "kicker_name", "kicker_quality")

set.seed(123)
train_index <- createDataPartition(data$success, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

#1st Model
linear_model <- lm(success ~ yardline + kicker_quality, data = train_data)
summary(linear_model)

#2nd Model

logistic_model <- glm(success ~ yardline + kicker_quality, data = train_data, family = "binomial")
summary(logistic_model)
log_odds_change = coefficient
odds_ratio = exp(coefficient)


#3rd Model
tree_model <- rpart(success ~ yardline + kicker_quality, 
                    data = data, 
                    method = "class", 
                    control = rpart.control(cp = 0.005)) 
rpart.plot(tree_model, type = 3, extra = 104)

accuracy <- function(predictions, actuals) {
  pred_class <- ifelse(predictions >= 0.5, 1, 0)
  mean(pred_class == actuals)
}

linear_pred <- predict(linear_model, test_data)
linear_acc <- accuracy(linear_pred, test_data$success)

logistic_pred <- predict(logistic_model, test_data, type = "response")
logistic_acc <- accuracy(logistic_pred, test_data$success)

tree_pred <- predict(tree_model, test_data, type = "prob")[, 2]
tree_acc <- accuracy(tree_pred, test_data$success)

print(paste("Model1 Accuracy:", round(linear_acc, 3)))
print(paste("Model2 Accuracy:", round(logistic_acc, 3)))
print(paste("Model3 Accuracy:", round(tree_acc, 3)))

pred_obj <- prediction(logistic_pred, test_data$success)
perf <- performance(pred_obj, "tpr", "fpr")
plot(perf, main = "Logistic Reg. ROC")
abline(a = 0, b = 1, lty = 2)

summary(logistic_model)$coefficients
