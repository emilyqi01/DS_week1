calculate_geometric_mean <- function(numbers) {
  
  if (length(numbers) == 0) {
    stop("Input vector is empty.")
  }
  
  if (any(numbers <= 0)) {
    stop("All elements in the vector must be positive for geometric mean calculation.")
  }
  
  result <- exp(mean(log(numbers)))
  return(result)
}

# Example usage:
numeric_vector <- c(2, 4, 8, 16, 32)
geometric_mean_result <- calculate_geometric_mean(numeric_vector)
print(paste("Geometric Mean:", geometric_mean_result))

calculate_rolling_arithmetic_mean <- function(numbers, window_size) {
  if (length(numbers) == 0) {
    stop("Input vector is empty.")
  }
  
  if (window_size <= 0 || window_size > length(numbers)) {
    stop("Invalid window size.")
  }
  
  result <- numeric(length = length(numbers) - window_size + 1)
  
  for (i in 1:(length(numbers) - window_size + 1)) {
    result[i] <- mean(numbers[i:(i + window_size - 1)])
  }
  
  return(result)
}

# Example usage:
numeric_vector <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
window_size <- 3
rolling_mean_result <- calculate_rolling_arithmetic_mean(numeric_vector, window_size)
print(paste("Rolling Arithmetic Mean:", rolling_mean_result))
