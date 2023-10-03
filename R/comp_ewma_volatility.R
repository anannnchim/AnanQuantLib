#' Compute Exponentially Weighted Moving Average (EWMA) Volatility
#'
#' This function computes the Exponentially Weighted Moving Average (EWMA) volatility
#' of a given stock's returns. The function uses a decay factor to assign more weight to
#' recent observations. The calculation is based on the square of daily returns.
#'
#' @param stock_data An xts object containing the stock's closing prices.
#' @param window An integer specifying the look-back window for the exponential decay factor.
#'               The default is set to 36.
#' @return An xts object containing the computed EWMA volatility values for each date
#'         in the original dataset, rounded to four decimal places.
#' @importFrom xts reclass
#' @importFrom quantmod dailyReturn
#' @examples
#' stock_data = xts(runif(100, 100, 200), Sys.Date() + 1:100)
#' comp_ewma_volatility(stock_data)
#' comp_ewma_volatility(stock_data, window = 50)
comp_ewma_volatility <- function(stock_data, window = 36) {

  # Initialize decay factor based on the look-back window
  decay_factor <- (2 / (window + 1))

  # Compute the square of daily returns
  return_power_two <- dailyReturn(stock_data)^2

  # Initialize a numeric vector to store variance values
  variance <- numeric(nrow(stock_data))

  # Loop through the stock data to compute EWMA volatility
  for(i in 1:nrow(stock_data)){
    if(i == 1){
      # First entry is NA as we start from the second data point
      variance[i] = NA
    }else if(i == 2){
      # Second entry is the square of the daily return at the second data point
      variance[i] = return_power_two[i]
    }else{
      # Compute variance based on the EWMA formula
      variance[i] = decay_factor * return_power_two[i] + (1 - decay_factor) * variance[i-1]
    }
  }

  # Convert the square root of variance to EWMA volatility and round to 4 decimal places
  # Return as an xts object
  return(reclass(round(sqrt(variance), 4), stock_data))
}

