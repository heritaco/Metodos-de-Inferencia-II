# Example data
data <- c(5, 10, 15, 20, 25)

# Define a function to calculate the mean
statistic <- function(x) {
  return(mean(x))
}

# Number of bootstrap samples
n_samples <- 1000
bootstrap_samples <- replicate(n_samples, sample(data, replace = TRUE))

# Calculate the statistic for each bootstrap sample
bootstrap_statistics <- apply(bootstrap_samples, 2, statistic)

# Analyze the distribution of the bootstrap statistics
mean_estimate <- mean(bootstrap_statistics)
ci <- quantile(bootstrap_statistics, c(0.025, 0.975))

# Print results
print(paste("Bootstrap Mean Estimate:", mean_estimate))
print(paste("95% Confidence Interval:", ci[1], "-", ci[2]))