#' Apply Binary Long-only EMA Crossover Buy/Sell Engine
#'
#' This function applies an Exponential Moving Average (EMA) crossover strategy to given stock data.
#' It calculates fast and slow EMAs based on specified parameters and generates binary long-only buy/sell signals
#' and hold states. The function augments the stock data with EMA values, signals, and hold states for each trading period.
#'
#' Signals and Execution:
#' - Signal {-1, 0, 1} is generated at the end of time (t) based on EMA crossover.
#' - Execution {-1, 0, 1} occurs at the end of time (t) + specified lag.
#' - Hold {1, 0} indicates whether to hold the position from (t+1) + lag until the next sell signal.
#' - A buy signal at time (t) leads to buying at the opening of (t+1), and a sell signal at time (t) leads to selling at the opening of (t+1).
#'
#' @param stock_data A data frame or xts object containing stock data, must include a 'Close' column.
#' @param ema_fast_param Integer specifying the period for the fast EMA.
#' @param ema_slow_param Integer specifying the period for the slow EMA.
#' @param lag Integer specifying the lag for the Hold state, default is 1.
#'
#' @return An xts object that includes the original stock data augmented with columns: Ema_fast, Ema_slow, Signal, Execution, and Hold.
#'
#' @examples
#' # Example usage:
#' # stock_data should be an xts object with a 'Close' column
#' result <- apply_ema_crossover_buysell_engine(stock_data, 12, 26)
#'
#' @export

apply_ema_crossover_buysell_engine <- function(stock_data, ema_fast_param, ema_slow_param, lag = 1){

  # Adjust columns name in case of one columns
  if(ncol(stock_data) == 1){
    names(stock_data) <- "Close"
  }

  # Create indicators
  Ema_slow = comp_ema(stock_data[, "Close"], ema_slow_param)
  Ema_fast = comp_ema(stock_data[, "Close"], ema_fast_param)

  # Add Signal
  Signal <- rep(0, length(Ema_fast))

  # Add dummy
  havePosition = 0

  for (i in 2:length(Ema_fast)) {

    if(Ema_fast[i] > Ema_slow[i] & havePosition == 0){

      if(Ema_fast[i-1] < Ema_slow[i-1] || Ema_fast[i-1] == Ema_slow[i-1]){
        Signal[i] <- 1
        havePosition = 1
      }
    }else if(Ema_fast[i] < Ema_slow[i] && havePosition == 1){

      if(Ema_fast[i-1] > Ema_slow[i-1] || Ema_fast[i-1] == Ema_slow[i-1]){
        Signal[i] <- -1
        havePosition = 0
      }
    }

  }

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

  # Add Lag of Hold
  Hold = Lag(Hold, lag)
  Hold[is.na(Hold)] = 0
  colnames(Hold) = "Hold"

  # Add Execution day
  Execution = Lag(Signal, lag)
  Execution[is.na(Execution)] = 0
  colnames(Execution) = "Execution"

  # Convert to xts
  Signal <- xts(Signal, order.by=index(stock_data))
  Execution <- xts(Execution, order.by=index(stock_data))
  Hold <- xts(Hold, order.by=index(stock_data))


  # Combine output
  output <- merge(stock_data, Ema_fast, Ema_slow ,Signal, Execution ,Hold)

  return(output)
}
