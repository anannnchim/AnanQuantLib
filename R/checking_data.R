#' Check Data for Common Issues
#'
#' This function checks a given stock data set for common data issues such as missing values (NA),
#' duplicated indices, and negative values. It counts the number of occurrences of these issues.
#'
#' @param stock_data An xts or data frame object containing stock data.
#'
#' @return A matrix with counts of various data issues:
#'         - 'number_of_NA': Number of missing values (NA) in the data.
#'         - 'number_of_duplication': Number of duplicated indices in the data.
#'         - 'number_of_negative': Number of negative values in the data.
#'
#' @examples
#' # Assuming stock_data is an xts or data frame object
#' issues <- checking_data(stock_data)
#' print(issues)
#'
#' @importFrom xts index
#' @importFrom base anyDuplicated
#' @importFrom base is.na
#' @importFrom base sum
#'
#' @export
checking_data <- function(stock_data) {

  number_of_rows = nrow(stock_data)
  number_of_NA <- sum(is.na(stock_data))
  number_of_duplication <- anyDuplicated(index(stock_data))
  number_of_negative <- sum(stock_data < 0, na.rm = TRUE)

  return(matrix(c(number_of_rows, number_of_NA, number_of_duplication, number_of_negative),
                nrow = 1,
                dimnames = list(c("Count"),
                                c("number_of_rows","number_of_NA", "number_of_duplication", "number_of_negative"))))
}
