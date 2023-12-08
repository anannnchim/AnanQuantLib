#' Apply EMA Crossover Buy/Sell Engine with ATR-based Condition
#'
#' This function applies an Exponential Moving Average (EMA) crossover strategy combined with
#' an Average True Range (ATR) based condition to the given stock data. It generates buy and
#' sell signals based on EMA crossover and modifies these signals further based on ATR conditions.
#' The function enhances the stock data with EMA values, signals, and hold states for each trading period.
#'
#' Signals and Execution:
#' - Signal {-1, 0, 1} is generated based on EMA crossover and ATR conditions.
#' - Execution {-1, 0, 1} occurs at the end of time (t) + specified lag.
#' - Hold {0, 1, 2} indicates the state of the position considering ATR conditions and EMA crossover.
#' - A buy signal leads to buying at the opening of the next period, and a sell signal leads to selling at the opening of the next period.
#' - Additional buy signals can increase the Hold state from 1 to 2 based on ATR conditions.
#'
#' @param stock_data A data frame or xts object containing stock data, must include 'Open' and 'Close' columns.
#' @param ema_fast_param Integer specifying the period for the fast EMA.
#' @param ema_slow_param Integer specifying the period for the slow EMA.
#' @param atr_multiple Numeric specifying the multiple of ATR to add to the last execution price for additional buy signals.
#' @param atr_lookback Integer specifying the period to calculate ATR (default 14).
#' @param lag Integer specifying the lag for the execution, default is 1.
#'
#' @return An xts object that includes the original stock data augmented with columns: Ema_fast, Ema_slow, Signal, Execution, and Hold.
#'
#' @examples
#' # Example usage:
#' # stock_data should be an xts object with 'Open' and 'Close' columns
#' result <- apply_ema_crossover_buysell_engine_continuous_atr(stock_data, 12, 26, 2, 14, 1)
#'
#' @export

apply_ema_crossover_buysell_engine_continuous_atr <- function(stock_data, ema_fast_param, ema_slow_param, atr_multiple, atr_lookback = 14, lag = 1) {

  # Create indicators
  Ema_slow <- EMA(stock_data[, "Close"], n = ema_slow_param) ; colnames(Ema_slow) <- "Ema_slow"
  Ema_fast <- EMA(stock_data[, "Close"], n = ema_fast_param) ; colnames(Ema_fast) <- "Ema_fast"
  Atr <- ATR(stock_data, n = atr_lookback)$atr

  # Initialize Signals and Holding
  Signal <- rep(0, length(Ema_fast))
  Hold <- rep(0, length(Ema_fast))
  havePosition <- 0
  buySignalCount <- 0
  last_execution_price <- NA


  # Get the next day from first non-NA
  first_non_NA_index = max(last(which(is.na(Ema_fast))), last(which(is.na(Ema_slow))), last(which(is.na(Atr)))) + 1

  # Loop
  for (i in first_non_NA_index:nrow(stock_data)) {

    # Skip iteration if any key variable is NA
    #if (is.na(Ema_fast[i]) || is.na(Ema_slow[i]) || is.na(Atr[i])) {
    #  next
    #}

    # Check for EMA crossover and position initiation
    if (Ema_fast[i] > Ema_slow[i] && havePosition == 0) {
      Signal[i] <- 1
      havePosition <- 1
      buySignalCount <- 1
      last_execution_price <- stock_data[i + lag, "Open"]
      Hold[i] <- 1
    } else if (Ema_fast[i] < Ema_slow[i] && havePosition > 0) {
      Signal[i] <- -1
      havePosition <- 0
      buySignalCount <- 0
      Hold[i] <- 0
    } else if (havePosition >= 1 && stock_data[i, "Close"] > ifelse( is.na(last_execution_price),NA, last_execution_price) + (atr_multiple * Atr[i]) && buySignalCount < 2) {
      Signal[i] <- 1
      buySignalCount <- buySignalCount + 1
      Hold[i] <- min(Hold[i-1] + 1, 2)
    } else {
      Hold[i] <- Hold[i - 1]
    }

    if (havePosition == 0) {
      last_execution_price <- NA
    }
  }

  # Apply lag to the signals and hold

  # Signal <- Lag(Signal, lag)
  # colnames(Signal) = "Signal"

  Execution <- Lag(Signal, lag)
  Execution[is.na(Execution)] = 0
  colnames(Execution) = "Execution"

  Hold <- Lag(Hold, lag)
  Hold[is.na(Hold)] = 0
  colnames(Hold) = "Hold"

  # Replace NA with zero
  Signal <- xts(Signal, order.by=index(stock_data))
  Execution <- xts(Execution, order.by=index(stock_data))
  Hold <- xts(Hold, order.by=index(stock_data))

  # Combine output
  output <- merge(stock_data, Ema_fast, Ema_slow, Signal, Execution, Hold)

  return(output)
}
