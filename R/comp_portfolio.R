#' Compute Portfolio using volatility per trade given stock_data, buy_sell_engine.
#'
#' This function computes the dynamics of a trading portfolio based on buy/sell signals,
#' stock data, and various trading parameters. It processes a series of trades as indicated
#' by a buy/sell signal engine and calculates the portfolio's value, cash position, invested
#' amount, and holdings over time. The function requires stock data with Open, High, Low,
#' and Close (OHLC) values and a compatible buy/sell signal engine.
#'
#' @param stock_data An xts object containing stock data with OHLC values.
#'        The data must include columns named 'Open', 'High', 'Low', and 'Close'.
#' @param buysell_engine An xts object representing buy/sell signals, where 1 indicates a buy
#'        signal, -1 indicates a sell signal, and 0 indicates no action.
#' @param initial_capital Numeric, the initial capital amount in the portfolio.
#' @param cost_per_trade Numeric, the cost per trade expressed as a proportion of the
#'        trade value (for example, 0.001 for 0.1% per trade).
#' @param volatility_per_trade Numeric, the volatility per trade expressed as a
#'        proportion of the trade value, used to determine the volume of each trade.
#'
#' @return A list containing two xts objects: 'transaction_data' and 'portfolio_data'.
#'         'transaction_data' includes columns for 'Volume', 'Execution_price',
#'         'Transaction', 'Average_cost', and 'Comission'.
#'         'portfolio_data' includes columns for 'Value', 'Cash', 'Invested', and 'Holding'.
#'
#' @examples
#' stock_data <- ... # xts object with OHLC data
#' buysell_engine <- ... # xts object with buy/sell signals
#' portfolio_result <- comp_portfolio(stock_data, buysell_engine,
#'                                   10000, 0.001, 0.02)
#'
#' @export
comp_portfolio <- function(stock_data, buysell_engine, initial_capital, cost_per_trade, volatility_per_trade) {

  # Add Volatility: Use rolling std 36 days.
  stock_data = merge(stock_data, comp_ewma_volatility(stock_data, 36))

  # Initialize transaction_data and portfolio_data
  transaction_data <- xts(matrix(ncol = 5, nrow = nrow(stock_data)), order.by = index(stock_data))
  colnames(transaction_data) <- c("Volume", "Execution_price", "Transaction","Average_cost", "Comission")

  portfolio_data  <- xts(matrix(ncol = 4, nrow = nrow(stock_data)), order.by = index(stock_data))
  colnames(portfolio_data) <- c("Value", "Cash", "Invested", "Holding")

  for(i in 2:nrow(stock_data)){

    # initiate input
    if(i == 2){
      # portfolio_data
      portfolio_data$Value[i] = initial_capital
      portfolio_data$Cash[i] = initial_capital
      portfolio_data$Invested[i] = 0
      portfolio_data$Holding[i] = 0

    }
    else{

      # IF SELL signal
      if(buysell_engine$Execution[i] == -1 && portfolio_data$Holding[i-1] != 0 ){


        # transaction_data
        transaction_data$Volume[i] = portfolio_data$Holding[i-1] * -1
        transaction_data$Execution_price[i] = stock_data$Open[i] * (1-cost_per_trade)
        transaction_data$Transaction[i] = portfolio_data$Holding[i-1] * ifelse(is.na( transaction_data$Execution_price[i]) ,NA, transaction_data$Execution_price[i] )
        transaction_data$Average_cost[i] = NA
        transaction_data$Comission[i] = (stock_data$Open[i] * ifelse(is.na(portfolio_data$Holding[i-1]),0,portfolio_data$Holding[i-1])) * cost_per_trade # problem

        # portfolio_data
        portfolio_data$Cash[i] = sum(portfolio_data$Cash[i-1], transaction_data$Transaction[i], na.rm = TRUE)
        portfolio_data$Holding[i] = 0
        portfolio_data$Invested[i] = stock_data$Close[i] * ifelse(is.na(portfolio_data$Holding[i]),0,portfolio_data$Holding[i])
        portfolio_data$Value[i] = sum(portfolio_data$Cash[i] + portfolio_data$Invested[i])


        # IF BUY signal
      }else if (buysell_engine$Execution[i] == 1) {
        # Calculate the intended volume of the transaction
        intended_volume <- round(((portfolio_data$Value[i - 1] * volatility_per_trade) / stock_data$Volatility[i - 1]), 0)
        intended_volume = ifelse(is.na(intended_volume), 0, intended_volume) # Avoid: missing value where TRUE/FALSE needed

        # Calculate the cost of the intended transaction including commission
        total_transaction_cost <- (stock_data$Open[i] * intended_volume) * (1 + cost_per_trade)
        previous_cash = ifelse(is.na(portfolio_data$Cash[i-1]), NA, portfolio_data$Cash[i-1])

        # !is.na(total_transaction_cost) # && total_transaction_cost <= portfolio_data$Cash[i - 1]
        # Check if there is sufficient cash for the transaction
        if (intended_volume > 0 && total_transaction_cost < previous_cash) {
          # Process buy transaction
          transaction_data$Volume[i] <- intended_volume
          transaction_data$Execution_price[i] <- stock_data$Open[i] * (1 + cost_per_trade)
          transaction_data$Transaction[i] <- (transaction_data$Volume[i] * -1) * ifelse(is.na(transaction_data$Execution_price[i]), NA, transaction_data$Execution_price[i])
          transaction_data$Average_cost[i] <- transaction_data$Execution_price[i]
          transaction_data$Comission[i] <- (stock_data$Open[i] * transaction_data$Volume[i]) * cost_per_trade

          # Update portfolio_data
          portfolio_data$Cash[i] <- sum(portfolio_data$Cash[i-1], transaction_data$Transaction[i], na.rm = TRUE)
          portfolio_data$Holding[i] <- sum(transaction_data$Volume[1:i], na.rm = TRUE)
          portfolio_data$Invested[i] <- stock_data$Close[i] * ifelse(is.na(portfolio_data$Holding[i]), 0, portfolio_data$Holding[i])
          portfolio_data$Value[i] <- sum(portfolio_data$Cash[i] + portfolio_data$Invested[i])
        } else {
          # No transaction takes place if the intended volume is zero or cash is insufficient
          # Copy the previous portfolio state
          portfolio_data[i, ] <- portfolio_data[i-1, ]
          transaction_data[i, ] <- transaction_data[i-1, ]
        }
      }
      else{

        # transaction_data
        transaction_data$Volume[i] = NA
        transaction_data$Transaction[i] = NA
        transaction_data$Average_cost[i] =  transaction_data$Average_cost[i-1]

        # portfolio_data
        portfolio_data$Cash[i] = sum(portfolio_data$Cash[i-1], transaction_data$Transaction[i], na.rm = TRUE)
        portfolio_data$Holding[i] =  sum(transaction_data$Volume[1:i], na.rm = TRUE)
        portfolio_data$Invested[i] = stock_data$Close[i] * ifelse(is.na(portfolio_data$Holding[i]),0,portfolio_data$Holding[i])
        portfolio_data$Value[i] = sum(portfolio_data$Cash[i] + portfolio_data$Invested[i])

      }


    }

  }

  return(list(transaction_data = transaction_data, portfolio_data = portfolio_data))
}
