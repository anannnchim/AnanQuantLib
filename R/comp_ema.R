#' Compute Exponential Moving Average
#'
#' This function calculates the Exponential Moving Average (EMA) of a given stock
#' based on the close prices.
#'
#' @param stock_data xts object containing stock data.
#' @param window Integer for the look-back window.
#' @return An xts object with the computed EMA.
#'
#' @examples
#' stock_data = xts(runif(100, 100, 200), Sys.Date() + 1:100)
#' comp_ema(stock_data, window = 20)

comp_ema = function(stock_data, window){

  # 1. Initialize variables
  decay_factor = ( 2 /( window + 1))
  rolling_ema = c()

  # 2. Compute EMA
  for(i in 1:nrow(stock_data)){
    if(i == 1){
      rolling_ema[i] = Cl(stock_data)[i]
    }else{
      rolling_ema[i] = (Cl(stock_data)[i] * decay_factor) + ((1-decay_factor) * rolling_ema[i-1] )
    }
  }

  # 3. Return the EMA
  return(reclass(round(rolling_ema,2), stock_data))
}
