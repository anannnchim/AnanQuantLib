# Load necessary libraries
library(httr)
library(jsonlite)
library(dplyr)

#' Download Selected Stock Data by Symbol from SETSMART API
#'
#' Retrieves selected end-of-day (EOD) stock price data for a given symbol from the SETSMART API.
#' The returned data includes date, symbol, open, high, low, close prices, as well as volume and value of trades.
#' The data availability period is subject to the constraints of the SETSMART package.
#'
#' @param api_key string The API key for authentication with the SETSMART API.
#' @param symbol string The stock symbol for which data is being requested.
#' @param startDate Date The start date for the data retrieval in 'YYYY-MM-DD' format.
#' @param endDate Date The end date for the data retrieval in 'YYYY-MM-DD' format.
#'
#' @return A dataframe containing the columns: date, symbol, open, high, low, close, aomVolume, aomValue.
#'         Each row represents a day's trading data for the specified symbol and date range.
#'         In case of an error or unavailable data, a warning is issued, and an empty dataframe is returned.
#'
#' @examples
#' api_key <- "your_api_key"
#' stock_quote <- download_stock_data_setsmart(api_key, "JTS", "2023-01-02", "2023-01-04")
#'
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr as.data.frame select

download_stock_data_setsmart <- function(api_key, symbol, startDate, endDate) {
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

    # Select only specified columns
    selected_data <- as.data.frame(parsed_data) %>%
      select(date, symbol, open, high, low, close, aomVolume, aomValue)

    return(selected_data)
  } else {
    warning("Failed to retrieve data: HTTP status code ", status_code(response))
    return(data.frame())
  }
}
