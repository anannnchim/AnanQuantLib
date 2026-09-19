#' Get End-of-Day Stock Data for Common Stocks from API
#'
#' Retrieves adjusted end-of-day prices for common stocks over a date range.
#' HTTP errors and malformed non-empty responses stop the update. Empty weekend
#' dates and possible weekday market holidays are reported separately.
#'
#' @param api_key API key used to authenticate with SETSMART.
#' @param from First requested date.
#' @param to Last requested date.
#' @return A data frame with date, symbol, OHLC, volume, and value columns.
#' @export
setsmart_get_database <- function(api_key, from, to) {
  required <- c("date", "symbol", "open", "high", "low", "close", "aomVolume", "aomValue")
  from <- as.Date(from)
  to <- as.Date(to)
  if (!is.character(api_key) || length(api_key) != 1L || !nzchar(api_key)) {
    stop("SETSMART API key is missing.")
  }
  if (is.na(from) || is.na(to) || from > to) {
    stop("Invalid SETSMART date range.")
  }

  url <- "https://www.setsmart.com/api/listed-company-api/eod-price-by-security-type"
  headers <- httr::add_headers(`api-key` = api_key)
  date_seq <- seq(from, to, by = "day")
  empty_weekends <- as.Date(character())
  empty_weekdays <- as.Date(character())

  all_data <- lapply(date_seq, function(date) {
    request_date <- as.Date(date, origin = "1970-01-01")
    response <- httr::GET(url, headers, query = list(
      securityType = "CS",
      date = format(request_date, "%Y-%m-%d"),
      adjustedPriceFlag = "Y"
    ))
    if (httr::status_code(response) != 200L) {
      stop("SETSMART request failed for ", request_date, " with HTTP ",
           httr::status_code(response), ".")
    }

    body <- httr::content(response, type = "text", encoding = "UTF-8")
    parsed <- tryCatch(jsonlite::fromJSON(body, flatten = TRUE),
                       error = function(error) {
                         stop("SETSMART returned invalid JSON for ", request_date,
                              ": ", conditionMessage(error))
                       })
    if (is.null(parsed) || length(parsed) == 0L ||
        (is.data.frame(parsed) && nrow(parsed) == 0L)) {
      if (weekdays(request_date) %in% c("Saturday", "Sunday")) {
        empty_weekends <<- c(empty_weekends, request_date)
      } else {
        empty_weekdays <<- c(empty_weekdays, request_date)
      }
      return(data.frame())
    }

    parsed <- as.data.frame(parsed)
    missing <- setdiff(required, names(parsed))
    if (length(missing)) {
      stop("SETSMART response for ", request_date,
           " is missing columns: ", paste(missing, collapse = ", "), ".")
    }
    dplyr::select(parsed, dplyr::all_of(required))
  })

  combined <- dplyr::bind_rows(all_data)
  if (length(empty_weekends)) {
    message("No SETSMART rows for weekend dates: ",
            paste(empty_weekends, collapse = ", "), ".")
  }
  if (length(empty_weekdays)) {
    message("No SETSMART rows for weekday dates (possible market holidays): ",
            paste(empty_weekdays, collapse = ", "), ".")
  }
  attr(combined, "empty_weekend_dates") <- empty_weekends
  attr(combined, "empty_weekday_dates") <- empty_weekdays
  combined
}
