# Load necessary libraries
library(httr)
library(jsonlite)
library(dplyr)

#' Get End-of-Day Stock Data for Common Stocks from API
#'
#' Retrieves end-of-day (EOD) stock price data for common stocks ('CS') over a specified date range. 
#' The data includes open, high, low, and close prices, as well as volume and value of trades.
#' This function automatically sets the security type to 'CS' and the adjusted price flag to 'Y'.
#'
#' @param api_key string The API key for authentication with the data source.
#' @param from Date The start date for the data retrieval in 'YYYY-MM-DD' format.
#' @param to Date The end date for the data retrieval in 'YYYY-MM-DD' format.
#'
#' @return A dataframe with columns: date, symbol, open, high, low, close, aomVolume, aomValue.
#'         Each row represents a day's trading data for a specific symbol within the given date range.
#'         If data for a specific date is unavailable or an error occurs, a warning is issued, and an empty dataframe is returned.
#'
#' @examples
#' api_key <- "your_api_key"
#' stock_data <- setsmart_get_database(api_key, "2021-01-01", "2021-01-31")
#'
#' @importFrom httr GET add_headers status_code content
#' @importFrom jsonlite fromJSON
#' @importFrom lubridate as.Date
#' @importFrom dplyr select bind_rows

setsmart_get_database <- function(api_key, from, to) {
  
  
  url <- "https://www.setsmart.com/api/listed-company-api/eod-price-by-security-type"
  headers <- httr::add_headers(`api-key` = api_key)
  
  # Generate a sequence of dates
  date_seq <- seq(as.Date(from), as.Date(to), by = "day")
  
  all_data <- lapply(date_seq, function(date) {
    params <- list(
      securityType = "CS",
      date = format(date, "%Y-%m-%d"),
      adjustedPriceFlag = "Y"
    )
    
    response <- httr::GET(url, headers, query = params)
    if (status_code(response) == 200) {
      data <- content(response, type = "text", encoding = "UTF-8")
      parsed_data <- fromJSON(data, flatten = TRUE)
      
      # Check if the expected columns are present
      if (!is.null(parsed_data) && all(c("date","symbol", "open", "high", "low", "close", "aomVolume", "aomValue") %in% names(parsed_data))) {
        selected_data <- as.data.frame(parsed_data) %>%
          select(date, symbol, open, high, low, close, aomVolume, aomValue)
        return(selected_data)
      } else {
        return(data.frame())
      }
    } else {
      warning("Failed to retrieve data for date ", format(date, "%Y-%m-%d"), ": HTTP status code ", status_code(response))
      return(data.frame())
    }
  })
  
  # Combine all data frames
  combined_data <- bind_rows(all_data)
  return(combined_data)
}
