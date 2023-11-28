#' Create Annual Performance Table
#'
#' This function computes an annual performance summary for a given set of daily returns.
#' The function uses the PerformanceAnalytics package to calculate various performance metrics.
#'
#' @param daily_return A time-series object or a numeric vector containing the daily returns of the asset.
#' @return A data frame containing the annualized performance metrics such as Annualized Returns,
#' Average Drawdown, and Maximum Drawdown.
#' @examples
#' # Example using hypothetical daily returns
#' daily_returns <- rnorm(252, 0, 0.02)
#' performance_table <- create_annual_performance_table(daily_returns)
#' print(performance_table)
#' @importFrom PerformanceAnalytics table.AnnualizedReturns AverageDrawdown maxDrawdown
create_annual_performance_table <- function(daily_return){

  # Compute metrics from PerformanceAnalytics package
  table <- table.AnnualizedReturns(daily_return, geometric = TRUE)
  table <- rbind(table, AverageDrawdown(daily_return))
  table <- rbind(table, "Max drawdown" = maxDrawdown(daily_return, geometric = TRUE))

  # Adjust data for presentation
  new_colnames <- paste("System", seq(1, ncol(daily_return)), sep = "_")
  colnames(table) <- new_colnames
  table <- round(table, 3)

  return(table)
}
