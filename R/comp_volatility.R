#' Compute Price Volatility
#'
#' This function calculates the price volatility of a given stock based on
#' the close prices using an exponential decay factor.
#'
#' @param stock_data xts object containing stock data.
#' @param window Integer for the look-back window.
#' @return An xts object with the computed price volatility.
#'
#' @examples
#' stock_data = xts(runif(100, 100, 200), Sys.Date() + 1:100)
#' comp_volatility(stock_data)

comp_volatility = function(stock_data, window = 36){

  # 1. Initialize variables
  decay_factor = ( 2 /( window + 1))
  return_power = (diff(Cl(stock_data))^2)
  variance = c()

  # 2. Compute the volatility
  for(i in 2:nrow(stock_data)){
    if(i == 2){
      variance[i] = return_power[i]
    }else{
      variance[i] = (return_power[i] * decay_factor) + ((1-decay_factor) * variance[i-1] )
    }
  }

  # 3. Return the volatility
  return(reclass(round(sqrt(variance),2), stock_data))
}
