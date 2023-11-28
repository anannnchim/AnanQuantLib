#' Apply a Random Buy/Sell Engine to Stock Data
#'
#' This function generates a random buy/sell signal for a given stock data based on the specified turnover rate per year.
#' It ensures an alternating pattern between buy (1) and sell (-1) signals, starting with a buy signal.
#' The function creates a series of buy and sell signals spread randomly over the trading period,
#' maintaining an average holding period corresponding to the specified turnover rate.
#'
#' @param stock_data An xts object containing the stock's data. Must have a 'Close' column.
#' @param turnover_per_year Numeric, representing the desired turnover rate per year.
#'
#' @return An xts object that includes the original stock data augmented with 'Signal', 'Execution', and 'Hold' columns.
#'         'Signal' represents the generated buy/sell signals, 'Execution' reflects the execution of these signals,
#'         and 'Hold' indicates whether the position is held (1) or not (0) after the execution of a signal.
#'
#' @examples
#' # Generate sample stock data
#' stock_data <- generate_continuous_price(0.0005, 0.0124, n = 1000)
#'
#' # Apply random buy/sell engine with a specified annual turnover rate
#' buysell_engine <- apply_random_buysell_engine(stock_data, turnover_per_year = 12)
#'
#' # View the resulting buy/sell signals
#' print(buysell_engine)
#'
#' @export
apply_random_buysell_engine <- function(stock_data, turnover_per_year) {


  num_periods <- nrow(stock_data)
  total_trades <- 2 * turnover_per_year

  # Initialize signal vector
  Signal <- rep(0, num_periods)

  # Randomly select entry dates for trades
  entry_dates <- sort(sample(num_periods, total_trades, replace = FALSE))

  # Ensure the first entry is a buy signal (1)
  current_signal <- 1

  for (date in entry_dates) {
    Signal[date] <- current_signal
    # Alternate the signal for the next trade
    current_signal <- -current_signal
  }

  # Create an xts object for the signals
  signal_xts <- xts(Signal, order.by = index(stock_data))

  # Add Hold
  Hold <- rep(0, NROW(stock_data))
  for (i in 2:NROW(stock_data)) {
    if (Signal[i] == 1) {
      Hold[i] <- 1
    } else if (Signal[i] == -1) {
      Hold[i] <- 0
    } else {
      Hold[i] <- Hold[i - 1]
    }
  }

  Execution = Signal
  names(Execution) = "Execution"

  # Combine
  output <- merge(stock_data,Signal,Execution,Hold)

  return(output)
}
