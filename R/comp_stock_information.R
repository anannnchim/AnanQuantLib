#' Compile Stock Information from Multiple Symbols
#'
#' This function gathers stock data for a given list of symbols over a specified date range.
#' It fetches the daily closing prices and computes the daily returns for each stock symbol.
#' The function is designed to handle cases where some stock data might not be available or encounters an error during fetching.
#'
#' @param universe A vector of stock symbols (tickers) for which the data needs to be fetched.
#' @param start_date The start date for the data fetch in "YYYY-MM-DD" format.
#' @param end_date The end date for the data fetch in "YYYY-MM-DD" format.
#'
#' @return A list containing two elements:
#'         - 'daily_return': A data frame (or an xts object) with columns representing the daily returns for each stock symbol.
#'         - 'close_price': A data frame (or an xts object) with columns representing the closing prices for each stock symbol.
#'
#' @examples
#' stocks_universe <- c("AAPL.US", "MSFT.US", "GOOGL.US")
#' stock_info <- comp_stock_information(stocks_universe, "2020-01-01", "2021-01-01")
#' head(stock_info$daily_return)
#' head(stock_info$close_price)
#'
#' @importFrom quantmod dailyReturn
#' @importFrom xts xts
#' @importFrom readr read_csv
#'
#' @export
comp_stock_information <- function(universe, start_date, end_date) {
  results <- list()
  results_two <- list()

  for (symbol in universe) {
    print(symbol)
    stock_data <- import_data_stooq(symbol, start_date, end_date)
    if (!is.null(stock_data)) {
      results[[symbol]] <- dailyReturn(stock_data$Close)
      results_two[[symbol]] <- stock_data$Close
    }
  }

  results_df <- do.call(cbind, results)
  results_df_two <- do.call(cbind, results_two)
  return(list(daily_return = results_df, close_price = results_df_two))
}
