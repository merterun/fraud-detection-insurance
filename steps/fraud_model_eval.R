#Model evaluation
set.seed(123)
trainIndex <- caret::createDataPartition(nfraud_oversampled$FraudFound_P, p = 0.7, list = FALSE)
train <- nfraud_oversampled[trainIndex,]
test <- nfraud_oversampled[-trainIndex,]

train$FraudFound_P <- as.factor(train$FraudFound_P)
test$FraudFound_P <- as.factor(test$FraudFound_P)


library(randomForest)
library(xgboost)

#gc()
#sessionInfo()


#library(usethis) 
#usethis::edit_r_environ() # R_MAX_VSIZE=3Gb 

# Train a random forest model
rf_model <- train(FraudFound_P ~ ., data = train, method = "rf", trControl = trainControl(method = "cv", number = 5))
rf_model


# Train an XGBoost model
xgb_model <- train(FraudFound_P ~ ., data = train, method = "xgbTree", trControl = trainControl(method = "cv", number = 5))
xgb_model



library(mlr)
library(mlr3)


train$FraudFound_P <- factor(train$FraudFound_P)
test$FraudFound_P <- factor(test$FraudFound_P, levels = levels(train$FraudFound_P))

# Predict on test set
rf_pred <- as.numeric(predict(rf_model, newdata = test))  # returns a numerical vector
#rf_pred <- predict(rf_model, newdata = test) # returns a numerical vector

rf_pred_binary <- ifelse(rf_pred > 0.5, "1", "0") # convert to binary factor with 2 levels
#rf_pred_binary <- ifelse(rf_pred > 0.5, 1, 0) # convert to binary vector with 2 levels
rf_pred_factor <- factor(rf_pred_binary, levels = levels(test$FraudFound_P))


xgb_pred <- predict(xgb_model, newdata = test)

# Evaluate performance
rf_perf <- confusionMatrix(rf_pred_factor, test$FraudFound_P)
xgb_perf <- confusionMatrix(xgb_pred, test$FraudFound_P)

# Print metrics
rf_perf$overall
xgb_perf$overall


#Generate a ROC curve and calculate the AUC score to evaluate the model's performance.
library(pROC)


# Generate ROC curve for XGBoost model
xgb_roc <- roc(test$FraudFound_P, predict(xgb_model, newdata = test, type = "prob")[,2])
plot(xgb_roc, main = "ROC Curve for XGBoost Model")
auc(xgb_roc)


# Extract confusion matrix values
tn <- xgb_perf$table[1,1]
fp <- xgb_perf$table[1,2]
fn <- xgb_perf$table[2,1]
tp <- xgb_perf$table[2,2]

# Calculate evaluation metrics
accuracy <- (tp + tn) / (tp + tn + fp + fn)
precision <- tp / (tp + fp)
recall <- tp / (tp + fn)
f1_score <- 2 * precision * recall / (precision + recall)

# Print the results
cat("Accuracy of xgb_model:", round(accuracy, 5), "\n")
cat("Precision of xgb_model:", round(precision, 5), "\n")
cat("Recall of xgb_model:", round(recall, 5), "\n")
cat("F1 Score of xgb_model:", round(f1_score, 5), "\n")

# Create a bar chart of evaluation metrics
metric_names <- c("Accuracy", "Precision", "Recall", "F1 Score")
metric_values <- c(accuracy, precision, recall, f1_score)

bar_data <- data.frame(metric_names, metric_values)

ggplot(bar_data, aes(x = metric_names, y = metric_values)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ylim(0, 1) +
  ggtitle("XGBoost Model Evaluation Metrics") +
  xlab("") +
  ylab("Metric Value") +
  geom_text(aes(label = round(metric_values, 2)), vjust = -0.5)


# Create confusion matrix
rf_cm <- table(rf_pred_factor, test$FraudFound_P)
rf_cm

# Calculate evaluation metrics
rf_accuracy <- sum(diag(rf_cm))/sum(rf_cm)   # Calculate accuracy as sum of diagonal elements divided by sum of all elements
rf_precision <- rf_cm[2,2]/sum(rf_cm[,2])    # Calculate precision as true positives divided by sum of predicted positives
rf_recall <- rf_cm[2,2]/sum(rf_cm[2,])       # Calculate recall as true positives divided by sum of actual positives
rf_f1score <- 2*rf_precision*rf_recall/(rf_precision+rf_recall)   # Calculate F1-score as harmonic mean of precision and recall

# Print metrics
cat("Random Forest Model Evaluation Metrics:\n")
cat(paste("Accuracy:", rf_accuracy, "\n"))     # Print accuracy
cat(paste("Precision:", rf_precision, "\n"))   # Print precision
cat(paste("Recall:", rf_recall, "\n"))         # Print recall
cat(paste("F1 Score:", rf_f1score, "\n"))      # Print F1-score


rf_cmv2 <- confusionMatrix(rf_pred_factor, test$FraudFound_P)    # Create confusion matrix using confusionMatrix function from caret package
rf_cmv2
