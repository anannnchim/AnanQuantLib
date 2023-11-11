library(xts)

#' Generate a time series of simulated asset prices using Gaussian distribution.
#'
#' This function generates a time series of asset prices with identical Open, High, Low, and Close values
#' starting from an initial equity of 100. It uses Gaussian-distributed percentage price changes to simulate the asset price movement.
#'
#' @param mean Numeric, the mean percentage change per time period (as a decimal, e.g., 0.01 for 1%).
#' @param sd Numeric, the standard deviation of the percentage change per time period (as a decimal, e.g., 0.03 for 3%).
#' @param n Integer, the number of time periods to simulate.
#' @return An xts object containing the simulated asset prices for each time period, with identical Open, High, Low, and Close values.
#' @examples
#' set.seed(123)
#' generate_continuous_price(0.01, 0.03, 662)
generate_continuous_price <- function(mean, sd, n) {
  n = n - 1 # Adjust for the initial value
  equity <- 100 # Initialize starting equity at 100

  # Generate Gaussian-distributed percentage price changes
  percent_changes <- rnorm(n, mean, sd)

  # Calculate the new price series
  for (i in 1:n) {
    equity <- c(equity, equity[i] * (1 + percent_changes[i]))
  }

  # Generate sequence of dates starting from '2021-01-01'
  dates <- seq(from = as.Date('2021-01-01'), by = 'days', length.out = n + 1)

  # Create an xts object with the simulated asset prices
  data_matrix <- matrix(rep(equity, 4), ncol = 4)
  colnames(data_matrix) <- c("Open", "High", "Low", "Close")
  xts_result <- xts(data_matrix, order.by = dates)

  # Return the resulting xts time series of simulated asset prices
  return(xts_result)
}



