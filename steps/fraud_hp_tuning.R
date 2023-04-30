
library(DiceKriging)
library(mlr3tuning)
library(rgenoud)

# Define the objective function to optimize (AUC)
objective_function <- function(data, param) {
  set.seed(123)
  xgb <- xgboost(
    data = data$train, 
    label = data$train$FraudFound_P,
    booster = "gbtree", 
    objective = "binary:logistic",
    eval_metric = "auc",
    nrounds = param$nrounds, 
    eta = param$eta, 
    max_depth = param$max_depth,
    subsample = param$subsample,
    colsample_bytree = param$colsample_bytree
  )
  
  pred <- predict(xgb, data$test)
  auc <- performance(prediction(pred, data$test$FraudFound_P), "auc")@y.values[[1]]
  
  return(list("measure" = auc, "params" = param))
}

# Define the parameter search space
params <- makeParamSet(
  makeIntegerParam("nrounds", lower = 1, upper = 100),
  makeNumericParam("eta", lower = 0.001, upper = 0.5),
  makeIntegerParam("max_depth", lower = 1, upper = 10),
  makeNumericParam("subsample", lower = 0.5, upper = 1),
  makeNumericParam("colsample_bytree", lower = 0.5, upper = 1)
)

# Define the tuning control
ctrl <- makeTuneControlMBO()

learner <- makeLearner("classif.xgboost", predict.type = "prob")

# Tune the hyperparameters using Bayesian optimization
tuned_params <- tuneParams(
  learner = learner,
  task = makeClassifTask(data = train, target = "FraudFound_P"),
  resampling = makeResampleDesc("CV", iters = 3),
  par.set = params,
  control = ctrl,
  show.info = TRUE,
  measures = list(auc)
)

tuned_params

