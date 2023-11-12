library(quantmod)
library(xts)

#' Compute Exponentially Weighted Moving Average (EWMA) Volatility in Price Terms
#'
#' This function computes the Exponentially Weighted Moving Average (EWMA) volatility
#' of a given stock's price changes, using a decay factor to assign more weight to recent observations.
#' The volatility is calculated based on the square of day-to-day price changes (absolute value). Simply, it is
#' daily volatility in price using standard deviation calculation method.
#'
#' @param stock_data An xts object containing the stock's closing prices.
#' @param window An integer specifying the look-back window for the exponential decay factor.
#'               The default value is 36.
#'
#' @return An xts object containing the computed EWMA volatility values for each date
#'         in the original dataset. The values are rounded to four decimal places.
#'
#' @examples
#' # Generate sample stock data
#' stock_data <- xts(runif(100, 100, 200), order.by = Sys.Date() - 100:1)
#'
#' # Calculate EWMA volatility with default window
#' volatility <- comp_ewma_volatility(stock_data)
#'
#' # Calculate EWMA volatility with a custom window
#' custom_volatility <- comp_ewma_volatility(stock_data, window = 50)
#'
comp_ewma_volatility <- function(stock_data, window = 36) {

  # Initialize decay factor based on the look-back window
  decay_factor <- (2 / (36 + 1))

  # Compute the daily price changes
  price_changes <- diff((stock_data$Close))

  # Square the daily price changes
  price_change_power_two <- price_changes^2

  # Initialize a numeric vector to store variance values
  variance <- numeric(nrow(stock_data))

  # Loop through the stock data to compute EWMA volatility
  for(i in 2:nrow(stock_data)) {
    if(i == 2){
      variance[i] = price_change_power_two[i]
    }else{
      variance[i] <- decay_factor * price_change_power_two[i] + (1 - decay_factor) * variance[i - 1]
    }

  }

  # Convert the square root of variance to EWMA volatility
  volatility <- reclass(round(sqrt(variance), 4), stock_data)
  colnames(volatility) <- "Volatility"
  volatility[1] <- NA

  return(volatility)
}
