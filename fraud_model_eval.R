#Model evaluation
set.seed(123)
trainIndex <- caret::createDataPartition(nfraud_oversampled$FraudFound_P, p = 0.7, list = FALSE)
train <- nfraud_oversampled[trainIndex,]
test <- nfraud_oversampled[-trainIndex,]

train$FraudFound_P <- as.factor(train$FraudFound_P)
test$FraudFound_P <- as.factor(test$FraudFound_P)


#Next, you can choose a suitable machine learning algorithm for this problem. 
#Since this is a classification problem with imbalanced classes, 
#you can consider using algorithms like Random Forest or XGBoost, which can handle imbalanced data well. You can use the train function from the caret package to train the model and tune the hyperparameters.


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



#library(mlr)
#library(mlrCPO)
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
cat("Accuracy:", round(accuracy, 2), "\n")
cat("Precision:", round(precision, 2), "\n")
cat("Recall:", round(recall, 2), "\n")
cat("F1 Score:", round(f1_score, 2), "\n")

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
rf_cmv2_table <- as.matrix(rf_cmv2$table)                         # Convert table from confusionMatrix into a matrix
rf_cmv2_df <- data.frame(Predicted = c(0, 1), Actual = c(0, 1), Count = c(rf_cmv2_table[1,1], rf_cmv2_table[2,1], rf_cmv2_table[1,2], rf_cmv2_table[2,2]))  # Create data frame for plotting the confusion matrix


ggplot(rf_cmv2_df, aes(x = Predicted, y = Actual, fill = as.numeric(Count))) +   # Create plot using ggplot2 package
  geom_tile() +                     # Add a tiled representation of the data
  scale_fill_gradient(low = "white", high = "steelblue", limits = c(0, max(rf_cm_df$Count, na.rm = TRUE))) +  # Color tiles using gradient scale
  geom_text(aes(label = Count), size = 10) +     # Add text labels to the tiles
  theme_minimal() +                 # Use minimal theme for the plot
  labs(x = "Predicted", y = "Actual", title = "Random Forest Confusion Matrix")  # Add labels for axes and title of the plot





