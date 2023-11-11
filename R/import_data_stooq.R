#' Import Data from Stooq
#'
#' This function imports historical stock data from Stooq and converts it to an xts object.
#' It formats the start and end dates to the required format and constructs the URL to download the data.
#'
#' @param symbol The ticker symbol of the stock.
#' @param start_date The start date for the data in 'YYYY-MM-DD' format.
#' @param end_date The end date for the data in 'YYYY-MM-DD' format.
#'
#' @return An xts object containing the stock data.
#'
#' @examples
#' \dontrun{
#' apple_stock <- import_data_stooq("AAPL.US", "2020-01-01", "2021-01-01")
#' }
#'
#' @export
#'
import_data_stooq <- function(symbol, start_date, end_date) {

  # Validate input dates
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  if(is.na(start_date) || is.na(end_date)) {
    stop("Invalid date format. Please use 'YYYY-MM-DD'.")
  }
  if(end_date < start_date) {
    stop("End date must be after start date.")
  }

  # Format the start and end dates to "YYYYMMDD".
  start_date <- format(start_date, "%Y%m%d")
  end_date <- format(end_date, "%Y%m%d")

  # Construct the URL for the required symbol and date range.
  url <- sprintf("https://stooq.com/q/d/l/?s=%s&d1=%s&d2=%s&i=d", symbol, start_date, end_date)

  # Import the data from the URL.
  stock_data <- readr::read_csv(url, show_col_types = FALSE)

  # Check if data is empty
  if (nrow(stock_data) == 0) {
    stop("No data available for the given symbol and date range.")
  }

  # Convert to 'xts' format
  tryCatch({
    stock_data_xts <- xts::xts(stock_data[,-1], order.by = as.Date(stock_data$Date))
  }, error = function(e) {
    stop("Error converting to xts format: ", e$message)
  })

  return(stock_data_xts)
}
