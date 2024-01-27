#' Import End-of-Day Stock Data from SETSMART
#'
#' Retrieves End-of-Day stock data from the SETSMART API for a specified symbol over a given date range.
#' The function returns data in an 'xts' format with columns for Open, High, Low, and Close prices.
#'
#' @param api_key string The API key for authenticating with the SETSMART API.
#' @param symbol string The stock symbol for which data is being requested.
#' @param startDate string The start date for the data retrieval in 'YYYY-MM-DD' format.
#' @param endDate string The end date for the data retrieval in 'YYYY-MM-DD' format.
#' @return An 'xts' object containing the columns: Date, Open, High, Low, Close.
#'         Each row represents a day's trading data for the specified symbol and date range.
#'         In case of an error or unavailable data, a warning is issued, and NULL is returned.
#' @examples
#' api_key <- "your_api_key"
#' stock_data <- import_data_setsmart(api_key, "TGE", "2024-01-04", "2024-01-15")
#' plot(stock_data)
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr select
#' @importFrom xts xts
#'

import_data_setsmart <- function(api_key, symbol, startDate, endDate) {

  library(httr)
  library(jsonlite)
  library(dplyr)
  library(xts)

  url <- "https://www.setsmart.com/api/listed-company-api/eod-price-by-symbol"

  # Headers and query parameters
  headers <- add_headers(`api-key` = api_key)
  params <- list(
    symbol = symbol,
    startDate = startDate,
    endDate = endDate,
    adjustedPriceFlag = "Y"
  )

  # API request
  response <- GET(url, headers, query = params)

  # Check response status
  if (status_code(response) == 200) {
    data <- content(response, type = "text", encoding = "UTF-8")
    parsed_data <- fromJSON(data, flatten = TRUE)

    # Convert data to a dataframe with capitalized column names and exclude unneeded columns
    selected_data <- as.data.frame(parsed_data) %>%
      select(date, Open = open, High = high, Low = low, Close = close)

    # Convert the data to 'xts' format
    if (nrow(selected_data) > 0) {
      selected_data$date <- as.Date(selected_data$date)
      xts_data <- xts(selected_data[,-1], order.by = selected_data$date)
      return(xts_data)
    } else {
      warning("No data available for the specified date range.")
      return(NULL)
    }
  } else {
    warning("Failed to retrieve data: HTTP status code ", status_code(response))
    return(NULL)
  }
}
