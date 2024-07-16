
# Setup Environment ----

library(tidyverse)
library(xgboost)
library(caret)
library(Metrics)


# Load data from feature prep
df = claimed_ride_feature_set

df = df %>% select(-c(trip_id)) # not a modeling feature


# Add a "random noise" feature as benchmark for feature gain evaluation
df$random_noise <- runif(nrow(df), min=.001, max=5) # Uniform random values between .01 and 5


# Split data into Train, Test, and Validation sets (80%, 10%, 10%)
trainIndex <- createDataPartition(df$claimed, p = 0.8, 
                                  list = FALSE, 
                                  times = 1)

dfTrain <- df[trainIndex, ]
temp <- df[-trainIndex, ]

testIndex <- createDataPartition(temp$claimed, p = 0.5, 
                                 list = FALSE, 
                                 times = 1)

dfTest <- temp[testIndex, ]
dfValidation <- temp[-testIndex, ]



# Prepare matrix formatting for XGBoost
dtrain <- xgb.DMatrix(data = as.matrix(dfTrain %>% select(-claimed)), label = dfTrain$claimed)
dtest <- xgb.DMatrix(data = as.matrix(dfTest %>% select(-claimed)), label = dfTest$claimed)
dvalidation <- xgb.DMatrix(data = as.matrix(dfValidation %>% select(-claimed)), label = dfValidation$claimed)




# Hyperparameter Tuning ----

# set.seed(414) 
best_mae <- Inf
best_params <- list()

for(i in 1:100) { # Run 100 iterations
  
  sample_params <- list(
    booster = "gbtree",
    objective = "binary:logistic",
    eta = runif(1, 0.01, 0.3),
    gamma = runif(1, 0, 5),
    max_depth = sample(3:20, 1),
    subsample = runif(1, 0.5, 1),
    colsample_bytree = runif(1, 0.5, 1),
    min_child_weight = runif(1, 1, 5)
  )
  
  model <- xgb.train(params = sample_params, 
                     data = dtrain, 
                     nrounds = 140,
                     watchlist = list(train = dtrain, test = dtest),
                     early_stopping_rounds = 10,
                     print_every_n = 10,
                     silent = 1)
  
  preds <- predict(model, dvalidation)
  current_mae <- mae(dfValidation$total_price, preds)
  
  if(current_mae < best_mae) {
    best_mae <- current_mae
    best_params <- sample_params
  }
}



# Prediction on validation set
preds <- predict(model, dvalidation)

# Get Mean Absolute Error of the validation preds
mae <- mae(dfValidation$claimed, preds)
print(paste("Mean Absolute Error on Validation Set:", mae))


# Get the tuning details
hyperparameters = model$params
hyperparameters_df <- data.frame(hyperparameter = names(hyperparameters), value = unlist(hyperparameters))

write.csv(hyperparameters_df , "hyperparaemters.csv", row.names = FALSE)

# Assess feature importance
importance_matrix <- xgb.importance(feature_names = colnames(dfTrain %>% select(-claimed)), model = model)
#plot
xgb.plot.importance(importance_matrix, measure = "Gain") # Features listed by Gain
# dataframe

importance_df <- as.data.frame(importance_matrix)
write.csv(importance_df , "feature_importance.csv", row.names = FALSE)


# Save the final model
saveRDS(model, "pred_claimed_ride.rds")

