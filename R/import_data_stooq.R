#' Import Data from Stooq
#'
#' This function imports historical stock data from Stooq, a financial data provider. It constructs
#' a URL for the given stock symbol and specified date range, then downloads and converts the data
#' into an xts object. The function handles any errors that may occur during data download or
#' conversion, such as issues with the symbol or data format.
#'
#' @param symbol The ticker symbol of the stock.
#' @param start_date The start date for the data in 'YYYY-MM-DD' format.
#' @param end_date The end date for the data in 'YYYY-MM-DD' format.
#'
#' @return An xts object containing the stock data if successful, or NULL in case of an error.
#'
#' @examples
#' \dontrun{
#'   # Example usage for Apple stock
#'   apple_stock <- import_data_stooq("AAPL.US", "2020-01-01", "2021-01-01")
#'   # Handle cases where data is not available or an error occurs
#'   if (is.null(apple_stock)) {
#'     message("Data not available or an error occurred.")
#'   }
#' }
#'
#' @export
import_data_stooq <- function(symbol, start_date, end_date) {
  # Format the start and end dates to "YYYYMMDD" as required by the URL.
  start_date_formatted <- format(as.Date(start_date), "%Y%m%d")
  end_date_formatted <- format(as.Date(end_date), "%Y%m%d")

  # Construct the URL for the required symbol and date range.
  url <- paste0("https://stooq.com/q/d/l/?s=", symbol, "&d1=", start_date_formatted, "&d2=", end_date_formatted, "&i=d")

  # Attempt to import the data from the URL.
  stock_data <- tryCatch({
    read_csv(url, show_col_types = FALSE)
  }, error = function(e) {
    warning("Failed to download data for symbol: ", symbol)
    return(NULL)
  })

  # Convert the data to 'xts' format if it's not NULL.
  if (!is.null(stock_data)) {
    return(tryCatch({
      as.xts(stock_data[,-1], order.by = as.Date(stock_data$Date))
    }, error = function(e) {
      warning("Failed to convert data to xts for symbol: ", symbol)
      return(NULL)
    }))
  } else {
    return(NULL)
  }
}

