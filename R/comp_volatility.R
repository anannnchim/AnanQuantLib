#' Compute Exponentially Weighted Moving Volatility
#'
#' This function computes the exponentially weighted moving volatility of a given stock
#' based on its closing prices. The exponential decay factor is utilized to give more weight
#' to recent observations.
#'
#' @param stock_data An xts object containing the stock's closing prices.
#' @param window An integer specifying the look-back window for the exponential decay factor.
#'               Default is set to 36.
#' @return An xts object containing the computed price volatility values for each date in the original dataset.
#' @importFrom xts reclass
#' @importFrom quantmod Cl
#' @examples
#' stock_data = xts(runif(100, 100, 200), Sys.Date() + 1:100)
#' comp_volatility(stock_data)
#' comp_volatility(stock_data, window = 50)

comp_volatility <- function(stock_data, window = 36) {

  # Initialize variables
  decay_factor <- (2 / (window + 1))
  return_power <- diff(Cl(stock_data))^2
  variance <- numeric(nrow(stock_data))

  # Compute the exponentially weighted moving volatility
  for(i in 2:nrow(stock_data)) {
    variance[i] <- if(i == 2) {
      return_power[i]
    } else {
      (return_power[i] * decay_factor) + ((1 - decay_factor) * variance[i - 1])
    }
  }

  # Return the volatility as an xts object
  return(reclass(round(sqrt(variance), 2), stock_data))
}
