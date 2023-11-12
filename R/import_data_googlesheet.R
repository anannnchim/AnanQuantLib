library(googlesheets4)
library(xts)

#' Read Stock Data from Google Sheets and Convert to xts Object
#'
#' This function reads stock data from a specified Google Sheet and converts it
#' into an xts object for time series analysis. The function requires the URL of
#' the Google Sheet and the name of the specific sheet within the document.
#'
#' @param sheet_url A string containing the URL of the Google Sheets document.
#' @param sheet_name A string specifying the name of the sheet within the Google Sheets document.
#'
#' @return An xts object containing the stock data, with the first column assumed to be dates.
#'
#' @examples
#' sheet_url <- "https://docs.google.com/spreadsheets/d/..."
#' stock_data <- read_stock_data_from_sheet(sheet_url, "Sheet1")
#'
#' @export
import_data_googlesheet <- function(sheet_url, sheet_name) {
  # Read the specified sheet from the Google Sheets document
  stock_data_raw <- read_sheet(sheet_url, sheet = sheet_name)

  # Convert the data to an xts object
  # Assumes the first column is 'Date' and is formatted as POSIXct
  stock_data_xts <- as.xts(stock_data_raw[,-1], order.by = as.POSIXct(stock_data_raw$Date))

  return(stock_data_xts)
}

# Example usage:
# sheet_url <- "https://docs.google.com/spreadsheets/d/1LxA-pSDfDmsH8rQzaeiS4aqyihUEcK7_bQng8i700MI/edit#gid=1963324788"
# stock_data <- import_data_googlesheet(sheet_url, "05_SET")
