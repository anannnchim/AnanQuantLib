# Load the xts package
library(xts)

#' Generate Discrete Price Series Based on a Payoff and Win Chance
#'
#' This function generates a discrete series of equity prices based on given
#' payoff and winning chance parameters. The function starts with an initial
#' equity of 100 and simulates each discrete step based on the provided
#' win_chance and payoff parameters.
#'
#' @param payoff Numeric, the amount won in each winning case.
#' @param win_chance Numeric, the probability of winning between 0 and 1.
#' @param n Integer, the number of time steps to simulate.
#' @return An xts object containing the equity values over the n time steps.
#' @examples
#' generate_discrete_price(payoff = 2, win_chance = 0.5, n = 100)
generate_discrete_price <- function(payoff, win_chance, n) {

  # Initialize variables
  outcomes <- c()
  equity <- 100
  result <- c(equity)  # Include the initial equity value

  # Generate the outcomes
  for (i in 1:n) {

    # Generate a random number between 0 and 1
    rand <- runif(1, min = 0, max = 1)

    # Determine the outcome based on the win_chance
    if (rand < win_chance) {
      outcomes <- c(outcomes, payoff)
    } else {
      outcomes <- c(outcomes, -1)
    }

    # Calculate the new equity value
    equity <- equity + outcomes[i]

    # Append the new equity value to the result vector
    result <- c(result, equity)
  }

  # Generate sequence of dates
  dates <- seq(from = as.Date('2021-01-01'), by = 'days', length.out = n + 1)

  # Create an xts object
  xts_result <- xts(result, order.by = dates)

  # Rename column
  colnames(xts_result) <- "Close"

  # Return the resulting xts object
  return(xts_result)
}
