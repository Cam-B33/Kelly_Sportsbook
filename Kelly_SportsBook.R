# Load required libraries
library(ggplot2)

# Final holding values to reach
final_values <- c(500, 1000, 2000, 5000, 10000, 20000, 50000, 100000)

# Starting bankroll and bet size
init_bankroll <- 50
bet_size <- 5

# Winning probability
p_win <- 0.5

# Initialize variables for the maximum geometric mean and the corresponding bet size
max_geometric_mean <- 0
max_bet_size <- 0

# Iterate through different bet sizes
for (bet_size in 1:50) {
  # Calculate probabilities of reaching each final value
  probs <- numeric(length(final_values))
  probs[1] <- 1
  for (i in 2:length(final_values)) {
    final_value <- final_values[i]
    prob <- 0
    for (n in 1:10000) {
      bankroll <- init_bankroll
      while (bankroll > 0 && bankroll < final_value) {
        if (runif(1, 0, 1) < p_win) {
          bankroll <- bankroll + 8 * bet_size - bet_size
        } else {
          bankroll <- bankroll - bet_size
        }
      }
      if (bankroll >= final_value) {
        prob <- prob + 1
      }
    }
    prob <- prob / 10000
    probs[i] <- prob
  }
  
  # Calculate the geometric mean of the final values
  geometric_mean <- exp(mean(log(final_values)))
  
  # If this bet size yields a higher geometric mean, update the maximum values
  if (geometric_mean > max_geometric_mean) {
    max_geometric_mean <- geometric_mean
    max_bet_size <- bet_size
  }
}

# Print the results
cat("The bet size that maximizes the geometric mean of the final value is", max_bet_size, "with a geometric mean of", max_geometric_mean)
