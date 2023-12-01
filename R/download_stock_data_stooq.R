#' Download and Store Stock Data for a Universe of Stocks
#'
#' This function iterates over a list of stock symbols (universe) and downloads
#' historical stock data for each symbol using the `import_data_stooq` function.
#' It handles cases where data for a specific symbol may not be available or an error
#' occurs during the download process. The data for each symbol is stored in a list.
#'
#' @param universe A vector of stock ticker symbols for which to download data.
#' @param start_date The start date for the data in 'YYYY-MM-DD' format.
#' @param end_date The end date for the data in 'YYYY-MM-DD' format.
#'
#' @return A list where each element is an xts object containing the stock data for a symbol.
#'         Symbols with failed data retrieval are excluded from the list.
#'
#' @examples
#' \dontrun{
#'   # Example usage for a set of stock symbols
#'   stocks_universe <- c("AAPL.US", "MSFT.US", "GOOGL.US")
#'   stock_data_list <- download_stock_data_stooq(stocks_universe, "2020-01-01", "2021-01-01")
#'
#'   # Accessing data for a specific stock
#'   if (!is.null(stock_data_list[["AAPL.US"]])) {
#'     apple_stock_data <- stock_data_list[["AAPL.US"]]
#'     # Perform operations with apple_stock_data
#'   }
#' }
#'
#' @export
download_stock_data_stooq <- function(universe, start_date, end_date) {
  stock_data_list <- list()

  for (symbol in universe) {
    message("Downloading data for: ", symbol)
    stock_data <- import_data_stooq(symbol, start_date, end_date)
    if (!is.null(stock_data)) {
      stock_data_list[[symbol]] <- stock_data
    }
  }

  return(stock_data_list)
}

