library(xts)

#' Generate Discrete Price Series with OHLC Values
#'
#' This function generates a discrete series of equity prices with Open, High,
#' Low, and Close values being identical, based on given payoff and winning
#' chance parameters. The function starts with an initial equity of 100 and
#' simulates each discrete step based on the provided win_chance and payoff
#' parameters. The resulting xts object will have OHLC columns, all with the
#' same values at each time step.
#'
#' @param payoff Numeric, the amount won in each winning case.
#' @param win_chance Numeric, the probability of winning between 0 and 1.
#' @param n Integer, the number of time steps to simulate.
#' @return An xts object containing OHLC values over the n time steps.
#' @examples
#' generate_discrete_price(payoff = 2, win_chance = 0.5, n = 100)
generate_discrete_price <- function(payoff, win_chance, n) {
  # Initialize variables
  outcomes <- c()
  equity <- 100
  ohlc_matrix <- matrix(nrow = n + 1, ncol = 4)  # Matrix for OHLC data

  # Set initial OHLC values
  ohlc_matrix[1, ] <- rep(equity, 4)

  # Generate the outcomes
  for (i in 2:(n + 1)) {
    # Generate a random number
    rand <- runif(1, min = 0, max = 1)

    # Determine the outcome
    if (rand < win_chance) {
      outcomes <- c(outcomes, payoff)
    } else {
      outcomes <- c(outcomes, -1)
    }

    # Update equity
    equity <- equity + outcomes[i - 1]

    # Set OHLC values
    ohlc_matrix[i, ] <- rep(equity, 4)
  }

  # Generate sequence of dates
  dates <- seq(from = as.Date('2021-01-01'), by = 'days', length.out = n + 1)

  # Create an xts object
  xts_result <- xts(ohlc_matrix, order.by = dates)

  # Set column names
  colnames(xts_result) <- c("Open", "High", "Low", "Close")

  # Return the resulting xts object
  return(xts_result)
}
