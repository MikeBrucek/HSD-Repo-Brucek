
# Setup Environment ----

library(tidyverse)
library(xgboost)
library(caret)
library(Metrics)


# Load data from feature prep
data = claimed_trip_df

df = data %>%
  select(-c(trip_id, price_change, boost_amount, hidden_charges)) %>% # not modeling features (no peeking)

# Add a "random noise" feature as benchmark for feature gain evaluation
df$random_noise <- runif(nrow(df), min=.001, max=5) # Uniform random values between .01 and 5




# Split data into Train, Test, and Validation sets (80%, 10%, 10%)
trainIndex <- createDataPartition(df$total_price, p = 0.8, 
                                  list = FALSE, 
                                  times = 1)

dfTrain <- df[trainIndex, ]
temp <- df[-trainIndex, ]

testIndex <- createDataPartition(temp$total_price, p = 0.5, 
                                 list = FALSE, 
                                 times = 1)

dfTest <- temp[testIndex, ]
dfValidation <- temp[-testIndex, ]

# Double-check that total_price is not in the feature set
if("total_price" %in% names(dfTrain %>% select(-total_price))) {
  stop("total_price is still in the feature set!")
}


# Prepare matrix formatting for XGBoost
dtrain <- xgb.DMatrix(data = as.matrix(dfTrain %>% select(-total_price)), label = dfTrain$total_price)
dtest <- xgb.DMatrix(data = as.matrix(dfTest %>% select(-total_price)), label = dfTest$total_price)
dvalidation <- xgb.DMatrix(data = as.matrix(dfValidation %>% select(-total_price)), label = dfValidation$total_price)




# Hyperparameter Tuning ----

# set.seed(414) 
best_mae <- Inf
best_params <- list()

for(i in 1:10) { # Run 100 iterations
  
  sample_params <- list(
    booster = "gbtree",
    objective = "reg:squarederror",
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
mae <- mae(dfValidation$total_price, preds)
print(paste("Mean Absolute Error on Validation Set:", mae))


# Get the tuning details
hyperparameters = model$params
hyperparameters_df <- data.frame(hyperparameter = names(hyperparameters), value = unlist(hyperparameters))

write.csv(hyperparameters_df , "hyperparaemters.csv", row.names = FALSE)

# Assess feature importance
importance_matrix <- xgb.importance(feature_names = colnames(dfTrain %>% select(-total_price)), model = model)
#plot
xgb.plot.importance(importance_matrix, measure = "Gain") # Features listed by Gain
# dataframe

importance_df <- as.data.frame(importance_matrix)
write.csv(importance_df , "feature_importance.csv", row.names = FALSE)



# Save the final model
saveRDS(model, "pred_accpeted_trip_price.rds")