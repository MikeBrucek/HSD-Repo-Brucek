# Install and load lpSolve package
# install.packages("lpSolve")
library(lpSolve)

# Define the optimization function
optimize_boost <- function(data) {
  # Extract the necessary data
  num_rides <- length(unique(data$trip_id))
  num_entries <- nrow(data)
  
  # Create a unique trip_id list
  trip_ids <- unique(data$trip_id)
  
  # Objective function: Minimize total price (base_price + boost_amount)
  objective <- data$total_price
  
  # Constraints: Each ride should be claimed at least once
  constraints <- matrix(0, nrow = num_rides, ncol = num_entries)
  for (i in 1:num_rides) {
    ride_indices <- which(data$trip_id == trip_ids[i])
    constraints[i, ride_indices] <- 1
  }
  
  # Right-hand side of the constraints
  rhs <- rep(1, num_rides)
  
  # Constraint directions
  dir <- rep(">=", num_rides)
  
  # Solve the linear program
  result <- lp("min", objective, constraints, dir, rhs, all.int = FALSE)
  
  # Extract the optimal boost amounts
  optimal_boosts <- result$solution
  
  # Incorporate randomness to make the boost offers less predictable
  set.seed(414)  # For reproducibility
  randomness <- runif(num_entries, min = -0.5, max = 0.5)
  optimal_boosts <- optimal_boosts + randomness
  
  return(list(optimal_boosts = optimal_boosts, total_cost = optimal_boosts + data$base_price))
}

# Example usage
# Assume `data` is already loaded as your dataset

test_ride1 = data %>% filter(trip_id == 1)
result <- optimize_boost(test_ride1)
print(result$optimal_boosts)
print(result$total_cost)
