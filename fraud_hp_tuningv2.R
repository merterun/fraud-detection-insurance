# Define tuning parameters
tuneGrid <- expand.grid(nrounds = c(100, 200),
                        max_depth = c(3, 5, 7),
                        eta = c(0.01, 0.1, 0.3),
                        gamma = c(0, 0.1, 1),
                        subsample = c(0.5, 0.8, 1),
                        colsample_bytree = c(0.5, 0.8, 1),
                        min_child_weight = c(1, 3, 5))


xgb_modelv2 <- train(FraudFound_P ~ ., data = train,
                     method = "xgbTree",
                     trControl = trainControl(method = "cv", number = 5),
                     tuneGrid = tuneGrid)

# Evaluate model performance
xgb_validation <- predict(xgb_modelv2, validation)
confusionMatrix(xgb_validation, validation$target_variable)