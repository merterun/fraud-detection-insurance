train$FraudFound_P <- as.numeric(train$FraudFound_P)
train$FraudFound_P[train$FraudFound_P == 1] <- 0
train$FraudFound_P[train$FraudFound_P == 2] <- 1

unique(train$FraudFound_P)

# Train an XGBoost model with the optimal hyperparameters
xgb_modelv3 <- xgboost(
  data = as.matrix(train[, -which(names(train) == "FraudFound_P")]), # exclude target variable
  label = train$FraudFound_P,
  nrounds = 80,
  eta = 0.157,
  max_depth = 5,
  subsample = 0.863,
  colsample_bytree = 0.768,
  objective = "binary:logistic"
)


summary(xgb_modelv3)

# Show variable importance and plot
importance_matrix <- xgb.importance(names(train[, -1]), model = xgb_modelv3)
summary(importance_matrix)
str(importance_matrix)


xgb.plot.importance(importance_matrix)
