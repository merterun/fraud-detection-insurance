train$FraudFound_P <- as.numeric(train$FraudFound_P)


# Train an XGBoost model with the optimal hyperparameters
xgb_modelv3 <- xgboost(
  data = as.matrix(train[, -which(names(train) == "FraudFound_P")]), # exclude target variable
  label = train$FraudFound_P,
  nrounds = 79,
  eta = 0.115,
  max_depth = 10,
  subsample = 0.939,
  colsample_bytree = 0.785,
  objective = "binary:logistic"
)


summary(xgb_modelv3)

# Show variable importance and plot
importance_matrix <- xgb.importance(names(train[, -1]), model = xgb_modelv3)
summary(importance_matrix)
str(importance_matrix)
importance_matrix


xgb.plot.importance(importance_matrix)
